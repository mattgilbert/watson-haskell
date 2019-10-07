## Watson-haskell

A partial reimplementation of [watson](http://tailordev.github.io/Watson/) in Haskell. This project is just a vehicle for me to learn Haskell, but the goal is behavior that is the same as watson.

Watson commands in bold have been implemented.

add
aggregate
**cancel**
config
edit
frames
**help**
log
merge
**projects**
remove
rename
**report**
restart
**start**
**status**
**stop**
sync
tags

Todo:
- tags command
- restart command
- report
  - output type: json, csv
  - limit to project name
  - limit to specific tags
- refactor/improve
  - parseWatsonTags
  - move aeson instances to separate file
  - change UUID to UUID' and get rid of silly GUID prefix
  - tags: State uses Maybe [], frame uses []... get these consistent
- create frames file if it doesn't exist
- improved error handling: e.g. corrupted frames/state
