rm -rf plutus-purs
mkdir plutus-purs
cd plutus-purs
git init
git remote add origin -f https://github.com/input-output-hk/plutus
git config core.sparseCheckout true
echo 'web-common-plutus/*' >> .git/info/sparse-checkout
echo 'web-common/*' >> .git/info/sparse-checkout
git pull origin e2cd641501d13715120329092b3a93df35493a44  # plutus-pab/v0.0.2
