# Parralel Toast

TODO add logo

## Overview

This project is a fun little Haskell application designed for concurrent processing of numerical operations on data streams that are read from files. It uses configuration files to define jobs and supports multi-threaded execution.

## Features

- Concurrent job execution
- Streaming data processing using Conduit
- JSON-based configuration
- Configurable numerical operations
- Basic logging and error handling


## Usage

1. Create a `config.json` with your config (example below).
2. Run the application:

```
stack run -- [optional_config_path]
```

If no config path is provided, it defaults to `config.json`.

Tests can be run using:
```
stack test
```


## Configuration

Example config file:

```json
{
  "jobs": [
    {
      "jobId": 1,
      "operations": [
        "Add",
        "Subtract",
        "Multiply",
        "Divide"
      ],
      "inFile": "in_example.txt",
      "outFile": "out_example.txt"
    },
    {
      "jobId": 777,
      "operations": [
        "Add",
        "Add"
      ],
      "inFile": "i1.txt",
      "outFile": "o1.txt"
    },
    {
      "jobId": 142,
      "operations": [
        "Add",
        "Subtract"
      ],
      "inFile": "i2.txt",
      "outFile": "o2.txt"
    }
  ],
  "logFile": "log.txt",
  "numberOfThreads": 4
}
```

## Large example

We can genarate 50 files:
`for j in {1..50}; do for i in {1..500000}; do echo $i; done > i$j.txt; done`

Then use the provided `configLarge.json` for testing.

