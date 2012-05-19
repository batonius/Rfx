{-# LANGUAGE ScopedTypeVariables,ExistentialQuantification #-}
import Graphics.UI.Gtk -- hiding (fill)
import Graphics.UI.Gtk.SourceView
import Control.Monad
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Error
import Control.Exception 
import Prelude hiding (catch, readFile, writeFile)
import System.IO.UTF8 hiding(putStrLn)
    
onlyLexer :: String -> String
onlyLexer = show . lexString

onlyParser :: String -> String
onlyParser = show . parseProgram . lexString

onlyValidator :: String -> String
onlyValidator = show . validateProgram . parseProgram . lexString
                
fullCompiler :: String -> String
fullCompiler = compileProgram defaultCompilerOptions . validateProgram . parseProgram . lexString
                
compileString :: Int -> String -> String
compileString index = [fullCompiler, onlyValidator, onlyParser, onlyLexer] !! index

compileBuffers combo rfxSourceView resSourceView = do
    rfxBuffer <- textViewGetBuffer rfxSourceView
    resBuffer <- textViewGetBuffer resSourceView
    startIter <- textBufferGetStartIter rfxBuffer
    endIter <- textBufferGetEndIter rfxBuffer
    textBufferRemoveTagByName rfxBuffer "Error" startIter endIter
    inText <- get rfxBuffer textBufferText
    index <- get combo comboBoxActive
    let compiledText = compileString index inText
    set resBuffer [textBufferText := compiledText]
            `Control.Exception.catch`
             (\ (ex::RfxException) -> do
                let line = getErrorLine ex
                lineIter <- textBufferGetIterAtLineOffset rfxBuffer (line-1) 0
                nLineIter <- textBufferGetIterAtLineOffset rfxBuffer (line) 0
                textBufferApplyTagByName rfxBuffer "Error" lineIter nLineIter
                set resBuffer [textBufferText := (show ex)] )
            `Control.Exception.catch`
             (\ (ex::SomeException) -> do
                set resBuffer [textBufferText := "Something wrong: " ++ (show ex)] )

withFileChooser :: String -> Window -> FileChooserAction -> (FilePath -> IO ()) -> IO ()
withFileChooser caption parent action work = do
  fileChooser <- fileChooserDialogNew (Just caption)
                  (Just parent) action [("Ok", ResponseOk)
                                      ,("Cancel", ResponseCancel)]
  response <- dialogRun fileChooser
  widgetHide fileChooser
  if response == ResponseOk
    then do
        maybePath <- fileChooserGetFilename fileChooser
        case maybePath of
          (Just path) -> work path
          Nothing -> return ()
    else return ()
    
main = do
  initGUI
  window <- windowNew
  hBox <- hBoxNew False 2
  vBox <- vBoxNew False 2
  quitButton <- buttonNewWithLabel "Quit"
  openButton <- buttonNewWithLabel "Open.."
  saveButton <- buttonNewWithLabel "Save.."
  autoCompileCheckButton <- checkButtonNewWithLabel "Auto compile"
  set autoCompileCheckButton [ toggleButtonActive := True ]
  compileButton <- buttonNewWithLabel "Compile"
  outputComboBox <- comboBoxNewText
  mapM_ (comboBoxAppendText outputComboBox)
       ["Compiler", "Validator", "Parser", "Lexer"]
  comboBoxSetActive outputComboBox 0
  buttonBox <- hButtonBoxNew
  languageManager <- sourceLanguageManagerNew
  cLanguage <- sourceLanguageManagerGetLanguage languageManager "c"
  resSourceView <- case cLanguage of
    (Just cLang) -> do
      buffer <- sourceBufferNewWithLanguage cLang
      sourceViewNewWithBuffer buffer
    Nothing -> sourceViewNew
  set resSourceView [ textViewWrapMode := WrapWord ]
  rfxLanguage <- sourceLanguageManagerGetLanguage languageManager "rfx"
  rfxSourceView <- case rfxLanguage of
    (Just rfxLang) -> do
      buffer <- sourceBufferNewWithLanguage rfxLang
      set buffer [ textBufferText := "thread a where\n\tstate b where\n\tend;\nend;"] 
      sourceViewNewWithBuffer buffer
    Nothing -> sourceViewNew
  set rfxSourceView [ textViewWrapMode := WrapWord ]
  rfxBuffer <- textViewGetBuffer rfxSourceView
  errorTag <- textTagNew (Just "Error")
  set errorTag [ textTagBackground := "#faa" ]
  tagTable <- textBufferGetTagTable rfxBuffer
  textTagTableAdd tagTable errorTag
  fd <- fontDescriptionNew
  fontDescriptionSetFamily fd "Consolas"
  fontDescriptionSetSize fd 10
  widgetModifyFont rfxSourceView (Just fd)
  widgetModifyFont resSourceView (Just fd)
  sourceViewSetAutoIndent rfxSourceView True
--  sourceViewSetIndentOnTab rfxSourceView True
  sourceViewSetTabWidth rfxSourceView 4
  sourceViewSetShowLineNumbers rfxSourceView True
  --
  buttonBoxSetLayout buttonBox ButtonboxStart
  mapM_ (boxPackStartDefaults buttonBox)
        [openButton, saveButton, compileButton
        , toButton autoCompileCheckButton]
  boxPackStartDefaults buttonBox outputComboBox
  boxPackStartDefaults buttonBox quitButton 
  buttonBoxSetChildSecondary buttonBox quitButton True
  boxPackStart vBox buttonBox PackNatural 0
  rfxScrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy rfxScrolledWindow PolicyAutomatic PolicyAutomatic
  scrolledWindowAddWithViewport rfxScrolledWindow rfxSourceView
  resScrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy resScrolledWindow PolicyAutomatic PolicyAutomatic
  scrolledWindowAddWithViewport resScrolledWindow resSourceView
  boxPackStart hBox rfxScrolledWindow PackGrow 0
  boxPackStart hBox resScrolledWindow PackGrow 0
  boxPackStart vBox hBox PackGrow 0
  set window [ containerChild := vBox]
  window `on` sizeRequest $ return (Requisition 800 600)
  window `onDestroy` mainQuit
  quitButton `onClicked` mainQuit
  onClicked openButton
    $ withFileChooser "Open rfx file" window FileChooserActionOpen $ \path -> do
         fileString <- readFile path
         buffer <- textViewGetBuffer rfxSourceView
         set buffer [textBufferText := fileString]
  onClicked saveButton
    $ withFileChooser "Save rfx file" window FileChooserActionSave $ \path -> do
         buffer <- textViewGetBuffer rfxSourceView
         fileString <- get buffer textBufferText
         writeFile path fileString
  onClicked compileButton $ compileBuffers outputComboBox rfxSourceView resSourceView
  rfxBuffer <- textViewGetBuffer rfxSourceView
  onBufferChanged rfxBuffer $ do
    autoCompile <- get autoCompileCheckButton toggleButtonActive
    if autoCompile 
      then compileBuffers outputComboBox rfxSourceView resSourceView
      else return ()
  outputComboBox `on` changed $  compileBuffers outputComboBox rfxSourceView resSourceView
  widgetShowAll window
  mainGUI