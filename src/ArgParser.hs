module ArgParser (CommandLineArgs(..), Command(..), ReportDateRange(..), getArgs) where

import Options.Applicative
import TimeTracker

data Command 
    = Status
    | Start ProjectName (Maybe String)
    | Stop (Maybe String)
    | Cancel

    | Projects -- display list of projects
    | Report 
        (Maybe ReportDateRange)
        (Maybe Bool)   -- current/no-current

-- data SpecificRange = SpecificRange String String

-- need the separate type since these are mutually exclusive
-- TODO: integrate into Report constructor above somehow, and adjust arg parser
data ReportDateRange 
    = Specific String String
    | LastYear
    -- | LastMonth
    -- | Week
    -- | Day
    -- luna? really?


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
    Start <$> strArgument (metavar "PROJECT-NAME" <> help "name of project")
          <*> (optional $ strOption (long "at" <> help "Start time"))

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
    Report <$> (optional foo)
           <*> (optional $ switch (long "current" <> short 'c' <> help "Include current frame"))

foo :: Parser ReportDateRange
foo =
    (specificRangeParser <|> rangeOptionParser)

specificRangeParser :: Parser ReportDateRange
specificRangeParser =
    Specific <$> (strOption (long "from" <> help "Report start date"))
             <*> (strOption (long "to" <> help "Report end date"))

rangeOptionParser :: Parser ReportDateRange
rangeOptionParser =
    flag' LastYear (long "y" <> help "last year")

-- reportDateRangeArg :: Parser ReportDateRange
-- reportDateRangeArg =
--     (flag' LastYear (long "y")) 
--         <|> (flag' Specific specificRangeArg)
--         <|> pure LastYear
--          
-- 
-- specificRangeArg :: Parser SpecificRange
-- specificRangeArg = 
--     undefined
    {--
    Specific <$> (strOption (long "from" <> help "Report start date"))
                  <*> (strOption (long "to" <> help "Report end date"))
    --}
