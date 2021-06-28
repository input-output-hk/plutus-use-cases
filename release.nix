# for internal ci
{}:
let poke-dex = import ./use-case-2 {};
in {
  poke-dex-exe = poke-dex.exe;
  # TODO: add plutus-starter pkg
}
