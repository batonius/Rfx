{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.SourceView
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk.Buttons.Button
import Language.Rfx.Compiler
import Language.Rfx.Validator
import Language.Rfx.Lexer
import Language.Rfx.Parser
import Language.Rfx.Error
import Control.Exception 
import Prelude hiding (catch)
    
compileString :: String -> IO String    
compileString string = do
  evaluate  $ compileProgram defaultCompilerOptions
           $ validateProgram
           $ parseProgram
           $ lexString string

doCompile :: String -> IO String
doCompile string = (compileString string) `Control.Exception.catch`
                   (\ (ex::SomeException) -> return $ "Lol i catch: " ++ (show ex))
           
withFileChooser :: String -> Window -> FileChooserAction -> (FilePath -> IO ()) -> IO ()
withFileChooser caption parent action work = do
  fileChooser <- fileChooserDialogNew (Just caption)
                  (Just parent) action [("Ok", ResponseOk)
                                      ,("Cancel", ResponseCancel)]
  response <- dialogRun fileChooser
  if response == ResponseOk
    then do
      widgetHide fileChooser
      maybePath <- fileChooserGetFilename fileChooser
      case maybePath of
        (Just path) -> work path
        Nothing -> return ()
    else return ()
    
main = do
  initGUI
  window <- windowNew
  hBox <- hBoxNew True 2
  vBox <- vBoxNew False 2
  quitButton <- buttonNewWithLabel "Quit"
  openButton <- buttonNewWithLabel "Open.."
  saveButton <- buttonNewWithLabel "Save.."
  compileButton <- buttonNewWithLabel "Compile"
  buttonBox <- hButtonBoxNew
  languageManager <- sourceLanguageManagerNew
  cLanguage <- sourceLanguageManagerGetLanguage languageManager "c"
  resSourceView <- case cLanguage of
    (Just cLang) -> do
      buffer <- sourceBufferNewWithLanguage cLang
      sourceViewNewWithBuffer buffer
    Nothing -> sourceViewNew
  rfxLanguage <- sourceLanguageManagerGetLanguage languageManager "rfx"
  rfxSourceView <- case rfxLanguage of
    (Just rfxLang) -> do
      buffer <- sourceBufferNewWithLanguage rfxLang
      sourceViewNewWithBuffer buffer
    Nothing -> sourceViewNew
  buttonBoxSetLayout buttonBox ButtonboxStart
  mapM_ (boxPackStartDefaults buttonBox)
        [openButton, saveButton, compileButton, quitButton]
  boxPackStart vBox buttonBox PackNatural 0
  boxPackStart hBox rfxSourceView PackGrow 0
  boxPackStart hBox resSourceView PackGrow 0
  boxPackStart vBox hBox PackGrow 0
  set window [ containerChild := vBox]
  window `on` sizeRequest $ return (Requisition 100 100)
  window `onDestroy` mainQuit
  quitButton `onPressed` mainQuit
  onPressed openButton
    $ withFileChooser "Open rfx file" window FileChooserActionOpen $ \path -> do
         fileString <- readFile path
         buffer <- textViewGetBuffer rfxSourceView
         set buffer [textBufferText := fileString]
  onPressed saveButton
    $ withFileChooser "Save rfx file" window FileChooserActionSave $ \path -> do
         buffer <- textViewGetBuffer rfxSourceView
         fileString <- get buffer textBufferText
         writeFile path fileString
  compileButton `onPressed` do
    rfxBuffer <- textViewGetBuffer rfxSourceView
    resBuffer <- textViewGetBuffer resSourceView
    inText <- get rfxBuffer textBufferText
    compiledText <- doCompile inText
    set resBuffer [textBufferText := compiledText]
  widgetShowAll window
  mainGUI    