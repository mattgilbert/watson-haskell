## Watson-haskell

A partial reimplementation of [watson](http://tailordev.github.io/Watson/) in Haskell. This project is just a vehicle for me to learn Haskell, but the goal is behavior that is the same as watson.

| Implemented | Todo  |
|--------|-------|
| cancel | add |
| help | aggregate |
| projects | config |
| report | edit |
| start | rename |
| status | log |
| stop | merge |
| frames | remove |
| restart | restart -S/-s, index, frame id |
| tags | sync |

Also, config isn't read, so no config options are currently respected.

### Todo
- report
  - output type: json, csv
  - limit to project name
  - limit to specific tags
- create frames file if it doesn't exist
- improved error handling: e.g. corrupted frames/state
