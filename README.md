



git -> job -> emitter



Git runner

* Sits just before work node
* Has many inputs
* Gets a ping:
    - Queries db to get input versions
    - Calculates execution checksum (downstram node/algo should be Hashable)
    - If it needs to run, then runs. Passes a callback downstream for output commits.
    - 'sit.





cabal install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include







TYPE GLOSSARY

n - node
e - emitter
r - record
s - state
i - input
m - monad
a - value
