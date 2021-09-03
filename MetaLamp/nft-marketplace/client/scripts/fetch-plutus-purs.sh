rm -rf plutus-purs
mkdir plutus-purs
cd plutus-purs
git init
git remote add origin -f https://github.com/input-output-hk/plutus
git config core.sparseCheckout true
echo 'web-common-plutus/*' >> .git/info/sparse-checkout
echo 'web-common/*' >> .git/info/sparse-checkout
git pull origin cc7bc06c4344cee6cd59bc170063fd627da25ed3  # plutus-starter-devcontainer/v1.0.8
