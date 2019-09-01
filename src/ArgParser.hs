module ArgParser (CommandLineArgs(..), Command(..), getArgs) where

import Options.Applicative
import TimeTracker

data Command 
    = Start ProjectName (Maybe String)
    | Stop (Maybe String)

data CommandLineArgs = CommandLineArgs Command

getArgs :: IO (Command)
getArgs = do
    (CommandLineArgs cmd) <- execParser (cmdLineArgsParser)
    return cmd

cmdLineArgsParser :: ParserInfo CommandLineArgs
cmdLineArgsParser = info 
    (helper <*> versionOption <*> commandCommandLineArgs)
    (fullDesc <> progDesc "watson" <> header "the header")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (long "version" <> help "Show version")

commandCommandLineArgs :: Parser CommandLineArgs
commandCommandLineArgs =
    CommandLineArgs <$> subparser (startCommand <> stopCommand)

startCommand :: Mod CommandFields Command
startCommand =
    command "start" (info startCommandLineArgs (progDesc "start tracking a project"))

startCommandLineArgs :: Parser Command
startCommandLineArgs =
    Start <$> strArgument (metavar "PROJECT-NAME" <> help "name of project")
          <*> (optional $ strOption (long "at" <> help "Start time"))

stopCommand :: Mod CommandFields Command
stopCommand =
    command "stop" (info stopCommandArgs (progDesc "stop tracking current project"))

stopCommandArgs :: Parser Command
stopCommandArgs =
    Stop <$> (optional $ strOption (long "at" <> help "Stop time"))

