let 
  sourcesFile = ./sources.json;
  system = builtins.currentSystem;
  sources = import ./sources.nix { inherit sourcesFile system; };
  plutus = import sources.plutus { };
  pkgs = plutus.pkgs;
in
  let
    cabalProjectParser = import "${sources."haskell.nix".outPath}/lib/cabal-project-parser.nix" { pkgs = plutus.pkgs; };

    projectFile = builtins.readFile ./../cabal.project;
    cabalProjectFileName = "cabal.project";
    lookupSha256 = _: null;

    blocks = pkgs.lib.splitString "\nsource-repository-package\n" ("\n" + projectFile);
    repoBlocks = builtins.map (pkgs.haskell-nix.haskellLib.parseBlock cabalProjectFileName
  lookupSha256) (pkgs.lib.lists.drop 1 blocks);
    sourceRepoData = pkgs.lib.lists.map (x: x.sourceRepo) repoBlocks;

    extractSourceNameForNiv = repoUrl:
      let matches = builtins.match "(.*github.com/(.+)/(.+)\.git)|(.*github.com/(.+)/(.+))" repoUrl;
          matchN = n: builtins.elemAt matches n ;

          owner = if matchN 2 == null then matchN 4 else matchN 1;
          repo  = if matchN 2 == null then matchN 5 else matchN 2;
      # in builtins.trace "matches = ${owner}/${repo}" repo;
      in repo;

    repos = builtins.map (repo: { name = extractSourceNameForNiv repo.url; tag = repo.ref; }) sourceRepoData;
  in repos
