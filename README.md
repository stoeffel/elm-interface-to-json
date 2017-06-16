elm-interface-to-json
=====================

> Reads all elm files of a project and returns a list of modules with their functions.

Installation
------------

```bash
$ npm i elm-interface-to-json -g
```

Usage
-----

```bash
$ elm-interface-to-json --path ./test/ # default path is current working dir
```

### Output

```json
[
   {
      "moduleName":"Foo.Bar",
      "types":[
         {
            "signature":"Int -> Int -> Int",
            "name":"moo"
         }
      ]
   },
   {
      "moduleName":"Main",
      "types":[
         {
            "signature":"Int -> Int -> Int",
            "name":"add"
         },
         {
            "signature":"Int -> Int -> Int",
            "name":"addHelper"
         }
      ]
   }
]
```
