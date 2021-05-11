ORMOLU_VERSION = 0.1.4.1
ORMOLU_OPTIONS = --mode inplace --check-idempotence

fmt:
	FMT=$$(stack exec --package ormolu-'$(ORMOLU_VERSION)' which ormolu) && \
		find src pab -type f -name \*.hs -exec \
			"$$FMT" $(ORMOLU_OPTIONS) '{}' +
