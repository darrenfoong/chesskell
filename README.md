# chesskell

## Formatting

```
ormolu -i $(find . -name '*.hs')
hlint src
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
