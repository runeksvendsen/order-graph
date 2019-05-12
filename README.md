[![CircleCI](https://circleci.com/gh/runeksvendsen/order-graph.svg?style=svg)](https://circleci.com/gh/runeksvendsen/order-graph)

# Running

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run the following commands:

```
git clone https://github.com/runeksvendsen/order-graph.git
cd order-graph/
stack setup
```

3. Install external dependencies

```
sudo apt-get install build-essential libgmp3-dev zlib1g-dev
```

4. Build CLI executable:

```
stack build
```

5. Grab a cup of coffee

6. Run CLI executable:

```
stack run -- <CLI arguments go here>
```

For example, to see the buy/sell liquidity of bitcoin in terms of US Dollars at 5% slippage using the small test data file `test/data/run1-1.json`, run the command:

```
stack run -- -n usd -c btc -s 5 --analyze test/data/run1-1.json
```

which will output:

```
Order book file          test/data/run1-1.json
Market (base/quote)      BTC/USD
Maximum slippage         5%
buy liquidity            18,050,706 USD
sell liquidity           16,228,938 USD
SUM                      34,279,645 USD
-----------------------------------------------------
```

# Input data

Input order books can be generated using the `WriteTestData` executable from [crypto-venues](https://github.com/runeksvendsen/crypto-venues).
