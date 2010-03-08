{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.SourceView
import Control.Monad.Trans(liftIO)
import Control.Monad
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Buttons.CheckButton
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Error
import Control.Exception 
import Prelude hiding (catch)
    
compileString :: String -> IO (Either (Int,String) String)
compileString string = do
  ret <- evaluate $ compileProgram defaultCompilerOptions
        $ validateProgram
        $ parseProgram
        $ lexString string
  return $ Right ret

doCompile :: String -> IO (Either (Int,String) String)
doCompile string = (compileString string) `Control.Exception.catch`
                   (\ (ex::RfxException) -> return $ Left ((getErrorLine ex), "Lol i catch: " ++ (show ex)))

compileBuffers rfxSourceView resSourceView = do
    rfxBuffer <- textViewGetBuffer rfxSourceView
    resBuffer <- textViewGetBuffer resSourceView
    startIter <- textBufferGetStartIter rfxBuffer
    endIter <- textBufferGetEndIter rfxBuffer
    textBufferRemoveTagByName rfxBuffer "Error" startIter endIter
    inText <- get rfxBuffer textBufferText
    compiledText <- doCompile inText
    case compiledText of
      Right text -> set resBuffer [textBufferText := text]
      Left (line, text) -> do
        lineIter <- textBufferGetIterAtLineOffset rfxBuffer (line-1) 0
        nLineIter <- textBufferGetIterAtLineOffset rfxBuffer (line) 0
        textBufferApplyTagByName rfxBuffer "Error" lineIter nLineIter
        set resBuffer [textBufferText := text]

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
  hPaned <- hPanedNew 
  vBox <- vBoxNew False 2
  quitButton <- buttonNewWithLabel "Quit"
  openButton <- buttonNewWithLabel "Open.."
  saveButton <- buttonNewWithLabel "Save.."
  autoCompileCheckButton <- checkButtonNewWithLabel "Auto compile"
  set autoCompileCheckButton [ toggleButtonActive := True ]
  compileButton <- buttonNewWithLabel "Compile"
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
      sourceViewNewWithBuffer buffer
    Nothing -> sourceViewNew
  set rfxSourceView [ textViewWrapMode := WrapWord ]
  rfxBuffer <- textViewGetBuffer rfxSourceView
  errorTag <- textTagNew (Just "Error")
  set errorTag [ textTagBackground := "#faa" ]
  tagTable <- textBufferGetTagTable rfxBuffer
  textTagTableAdd tagTable errorTag
  buttonBoxSetLayout buttonBox ButtonboxStart
  mapM_ (boxPackStartDefaults buttonBox)
        [openButton, saveButton, compileButton
        , toButton autoCompileCheckButton, quitButton]
  boxPackStart vBox buttonBox PackNatural 0
  panedPack1 hPaned rfxSourceView False False
  panedPack2 hPaned resSourceView False False
  boxPackStart vBox hPaned PackGrow 0
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
  onClicked compileButton $ compileBuffers rfxSourceView resSourceView
  rfxBuffer <- textViewGetBuffer rfxSourceView
  onBufferChanged rfxBuffer $ do
    autoCompile <- get autoCompileCheckButton toggleButtonActive
    if autoCompile 
      then compileBuffers rfxSourceView resSourceView
      else return ()
  widgetShowAll window
  mainGUI    