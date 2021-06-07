rm -rf plutus-purs
mkdir plutus-purs
cd plutus-purs
git init
git remote add origin -f https://github.com/input-output-hk/plutus
git config core.sparseCheckout true
echo 'web-common-plutus/*' >> .git/info/sparse-checkout
echo 'web-common/*' >> .git/info/sparse-checkout
git pull origin 58bf9ed626d498c140c69a859a508da03843d097
