echo "aspect melody peanut velvet exclude embrace embody candy muffin civil have pelican diary badge path inherit deny evolve tail actress galaxy guess amused glad" | cabal exec -- cardano-wallet key from-recovery-phrase Shelley  >> root.prv


cat root.prv \
  | cabal exec -- cardano-wallet key child 1852H/1815H/0H/0/0 \
  | tee address.prv \
  | cabal exec -- cardano-wallet key public --without-chain-code > address.pub


echo "root_xsk1fqsp4tpqx5ajtj6d35frajx0egrl304qtwugyd6hl3gmdddwxpy6u78e3y8guty0cr5q0xa5pyp0wz36kg9flh74e5gn9s2gtjl9d2l5288nhmmk4wm7vjv4rgnrkd58eq3q9e39547gvfvy5f8hdyshju22k7ld" \
  | cabal exec -- cardano-wallet key child 1852H/1815H/0H/0/0 \
  | tee address.prv \
  | cabal exec -- cardano-wallet key public --with-chain-code > address.pub

cat root.prv \
  | cabal exec -- cardano-wallet key child 1852H/1815H/0H \
  | tee account.prv \
  | cabal exec -- cardano-wallet key public --with-chain-code > account.pub

echo "root_xsk1fqsp4tpqx5ajtj6d35frajx0egrl304qtwugyd6hl3gmdddwxpy6u78e3y8guty0cr5q0xa5pyp0wz36kg9flh74e5gn9s2gtjl9d2l5288nhmmk4wm7vjv4rgnrkd58eq3q9e39547gvfvy5f8hdyshju22k7ld" \
  | cabal exec -- cardano-wallet key child 1852H/1815H/0H \
  | tee account.prv \
  | cabal exec -- cardano-wallet key public --with-chain-code > account.pub


cat address.pub | cabal exec --cardano-address address payment \
    --network-tag 1 > address.pay

## get displau address
echo "root_xsk1fqsp4tpqx5ajtj6d35frajx0egrl304qtwugyd6hl3gmdddwxpy6u78e3y8guty0cr5q0xa5pyp0wz36kg9flh74e5gn9s2gtjl9d2l5288nhmmk4wm7vjv4rgnrkd58eq3q9e39547gvfvy5f8hdyshju22k7ld" \
  | cabal exec -- cardano-wallet key child 1852H/1815H/0H/0/0 \
  | tee address.prv \
  | cabal exec -- cardano-wallet key public --with-chain-code > address-extended.pub

aspect melody peanut velvet exclude embrace embody candy muffin civil have pelican diary badge path inherit deny evolve tail actress galaxy guess amused glad




##Key converion 
 cabal exec -- cardano-cli key   convert-cardano-address-key --signing-key-file '/Users/volodymyr.derecha/projects/plutus/plutus-use-cases/eleks/oracle/cli-demo/keys/client/payment.skey'   --out-file test.json --shelley-payment-key

  cabal exec -- cardano-cli key   verification-key  --signing-key-file '/Users/volodymyr.derecha/projects/plutus/plutus-use-cases/eleks/oracle/cli-demo/keys/client/payment.skey'    --verification-key-file test.json 

    cabal exec -- cardano-cli key   non-extended-key  --extended-verification-key-file '/Users/volodymyr.derecha/projects/plutus/plutus-use-cases/eleks/oracle/cli-demo/keys/client/payment.vkey'   --verification-key-file test1.json



     cabal exec -- cardano-cli key   convert-cardano-address-key --signing-key-file '/Users/volodymyr.derecha/projects/plutus/plutus-use-cases/eleks/oracle/payment.skey'   --out-file test.json --shelley-payment-key
