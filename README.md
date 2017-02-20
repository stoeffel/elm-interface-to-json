elm-interface-to-json
=====================


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
