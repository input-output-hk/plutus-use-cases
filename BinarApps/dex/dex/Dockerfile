FROM nixos/nix:latest

RUN sed -i '$asubstituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' /etc/nix/nix.conf && \
    sed -i '$atrusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' /etc/nix/nix.conf

WORKDIR /build
COPY default.nix /build
COPY vendor /build/vendor
RUN ["nix-shell", "default.nix", "--run", "cabal update"]

COPY ./uniswap /build/uniswap
WORKDIR /build/uniswap
RUN ["nix-shell", "/build/default.nix", "--run", "cabal build all"]
