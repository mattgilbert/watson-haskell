module ArgParser (CommandLineArgs(..), Command(..), ReportDateRange(..), getArgs) where

import Options.Applicative
import TimeTracker

data Command 
    = Status
    | Start (Maybe String) ProjectName [String]
    | Stop (Maybe String)
    | Cancel

    | Projects -- display list of projects
    | Report 
        (Maybe ReportDateRange)
        (Maybe Bool)   -- current/no-current
    deriving (Show)

data ReportDateRange 
    = Specific String String
    | LastYear
    | LastMonth
    | LastWeek
    -- | Day
    -- luna? really?
    deriving (Show)


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
    CommandLineArgs <$> subparser (
           statusCommand 
        <> startCommand 
        <> stopCommand 
        <> cancelCommand 
        <> projectsCommand
        <> reportCommand
    )

statusCommand :: Mod CommandFields Command
statusCommand = 
    command "status" (info (pure Status) (progDesc "status of current project tracking"))

startCommand :: Mod CommandFields Command
startCommand =
    command "start" (info startCommandLineArgs (progDesc "start tracking a project"))

startCommandLineArgs :: Parser Command
startCommandLineArgs =
    Start <$> (optional $ strOption (long "at" <> help "Start time"))
          <*> strArgument (metavar "PROJECT-NAME" <> help "name of project")
          <*> (many (strArgument (help "tags")))

stopCommand :: Mod CommandFields Command
stopCommand =
    command "stop" (info stopCommandArgs (progDesc "stop tracking current project"))

stopCommandArgs :: Parser Command
stopCommandArgs =
    Stop <$> (optional $ strOption (long "at" <> help "Stop time"))

cancelCommand :: Mod CommandFields Command
cancelCommand = 
    command "cancel" (info (pure Cancel) (progDesc "cancel current project"))

projectsCommand :: Mod CommandFields Command
projectsCommand =
    command "projects" (info (pure Projects) (progDesc "list of all projects"))

reportCommand :: Mod CommandFields Command
reportCommand =
    command "report" (info (reportCommandArgs) (progDesc "generate a project report"))

reportCommandArgs :: Parser Command
reportCommandArgs =
    Report <$> (optional dateRangeParser)
           <*> (optional $ switch (long "current" <> short 'c' <> help "Include current frame"))

dateRangeParser :: Parser ReportDateRange
dateRangeParser =
    (specificRangeParser <|> rangeOptionParser)

specificRangeParser :: Parser ReportDateRange
specificRangeParser =
    Specific <$> (strOption (long "from" <> help "Report start date"))
             <*> (strOption (long "to" <> help "Report end date"))

rangeOptionParser :: Parser ReportDateRange
rangeOptionParser =
    flag' LastYear (short 'y'<> help "last year")
    <|> flag' LastMonth (short 'm'<> help "last month")
