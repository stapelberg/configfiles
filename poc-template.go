package main

import (
	"flag"
	"log"
	"os"
)

func logic() error {
	os.Open()
}

func main() {
	flag.Parse()
	if err := logic(); err != nil {
		log.Fatal(err)
	}
}
