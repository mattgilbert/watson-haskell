module ArgParser (CommandLineArgs(..), Command(..), ReportDateRange(..), getArgs) where

import Data.List
import Options.Applicative
import Base

data Command 
    = Status
    | Start (Maybe String) ProjectName [String]
    | Restart
    | Stop (Maybe String)
    | Cancel
    | Remove String
    | Edit String
    | Projects
    | Tags
    | Frames
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
    return (watsonifyArgs cmd)

watsonifyArgs :: Command -> Command
watsonifyArgs (Start at projName tags) =
    -- watson cli takes tags starting with + so that tag names can have
    -- spaces, *and* so can project name. optparse doesn't really work like
    -- that, so we'll monkey with the args here for now
    Start at watsonProjName watsonTags
    where
      (projNameRemainder, remainingTags) = span (\(c:_) -> c /= '+') tags
      watsonProjName = intercalate " " (projName:projNameRemainder)

      watsonTags = stripPlus <$> parseWatsonTags [] remainingTags
      stripPlus ('+':t) = t
      stripPlus t = t

watsonifyArgs cmd = cmd

parseWatsonTags :: [String] -> [String] -> [String]
parseWatsonTags actualTags [] = actualTags
parseWatsonTags actualTags (tagStart:remainingWords) =
    parseWatsonTags (actualTags ++ [(intercalate " " (tagStart:nextTagWords))]) remaining
    where
        (nextTagWords, remaining) = break (\(c:_) -> c == '+') remainingWords

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
        <> restartCommand
        <> stopCommand 
        <> cancelCommand 
        <> removeCommand
        <> editCommand
        <> projectsCommand
        <> tagsCommand
        <> framesCommand
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

restartCommand :: Mod CommandFields Command
restartCommand =
    command "restart" (info (pure Restart) (progDesc "restart most recent project"))

stopCommand :: Mod CommandFields Command
stopCommand =
    command "stop" (info stopCommandArgs (progDesc "stop tracking current project"))

stopCommandArgs :: Parser Command
stopCommandArgs =
    Stop <$> (optional $ strOption (long "at" <> help "Stop time"))

cancelCommand :: Mod CommandFields Command
cancelCommand = 
    command "cancel" (info (pure Cancel) (progDesc "cancel current project"))

removeCommand :: Mod CommandFields Command
removeCommand =
    command "remove" (info (removeCommandArgs) (progDesc "remove a frame"))

removeCommandArgs :: Parser Command
removeCommandArgs =
    Remove <$> (strArgument (metavar "frame" <> help "frame ID or index offset"))

editCommand :: Mod CommandFields Command
editCommand =
    command "edit" (info (editCommandArgs) (progDesc "edit a frame"))

editCommandArgs :: Parser Command
editCommandArgs =
    Edit <$> (strArgument (metavar "frame" <> help "frame ID or index offset"))

projectsCommand :: Mod CommandFields Command
projectsCommand =
    command "projects" (info (pure Projects) (progDesc "list of all projects"))

tagsCommand :: Mod CommandFields Command
tagsCommand =
    command "tags" (info (pure Tags) (progDesc "list of all tags"))

framesCommand :: Mod CommandFields Command
framesCommand =
    command "frames" (info (pure Frames) (progDesc "list all frame IDs"))

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
