[![CircleCI](https://circleci.com/gh/runeksvendsen/order-graph.svg?style=svg)](https://circleci.com/gh/runeksvendsen/order-graph)

# Running

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run the following commands:

```
git clone -n https://github.com/runeksvendsen/order-graph.git
cd order-graph/
stack setup
git checkout 999c1000a3b78c668b661ba60d599982d8eb5566
```

3. Build and run the executable:

```
stack run -- <CLI arguments go here>
```

For example, to see the buy/sell liquidity of bitcoin in terms of US Dollars at 5% slippage using the small test data file `test/data/run1-1.json`, run the command:

```
stack run -- -n usd -c btc -s 5 --analyze test/data/run1-40.json
```
