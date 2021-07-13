rm -rf plutus-purs
mkdir plutus-purs
cd plutus-purs
git init
git remote add origin -f https://github.com/input-output-hk/plutus
git config core.sparseCheckout true
echo 'web-common-plutus/*' >> .git/info/sparse-checkout
echo 'web-common/*' >> .git/info/sparse-checkout
git pull origin bd16cc29045ffc7eaa6beaabe3b985a56cb9292a  # plutus-starter-devcontainer/v1.0.6
