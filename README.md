## Watson-haskell

A partial reimplementation of [watson](http://tailordev.github.io/Watson/) in Haskell. This project is just a vehicle for me to learn Haskell, but the goal is behavior that is the same as watson.

| Implemented | Todo  |
|--------|-------|
| cancel | add |
| help | aggregate |
| projects | config |
| start | rename |
| status | log |
| stop | merge |
| remove | remove index, --force |
| restart | restart -S/-s, index, frame id |
| tags | sync |
| report | report --tag, --project, pager, color coding, include tags |
| edit | edit index, use $EDITOR |
| frames | |

Also, config isn't read, so no config options are currently respected.

### Todo
- improve messaging, add color coding
- report
  - output type: json, csv
  - limit to project name
  - limit to specific tags
- create frames file if it doesn't exist
- improved error handling: e.g. corrupted frames/state
