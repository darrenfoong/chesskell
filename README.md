# chesskell

## Formatting

```
ormolu --mode inplace $(find . -name '*.hs')
hlint .
```

## Building

```
stack build
```

## Running

```
stack run chesskell     # command line UI
stack run chesskell-web # web UI at localhost:3000
```
