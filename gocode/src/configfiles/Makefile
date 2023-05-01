.PHONY: update

all:

update:
	CGO_ENABLED=0 GOARCH=amd64 go build
	mv configfiles ../../bin
	CGO_ENABLED=0 GOARCH=arm64 go build
	mv configfiles ../../bin/configfiles.arm64
