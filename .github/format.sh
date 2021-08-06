# Extensions necessary to tell fourmolu about 
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
~/.local/bin/fourmolu --mode inplace --check-idempotence $EXTENSIONS $SOURCES
