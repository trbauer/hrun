default: hrun.exe

RBIN=C:/files/rbin

help:
	@echo "targets:   hrun.exe, TestIO.exe, install"
	
hrun.exe: HRun.hs Platform.hs
	ghc -threaded --make -o hrun.exe HRun.hs

TestIO.exe: OutputStreamTester.hs
	ghc -threaded --make -o TestIO.exe OutputStreamTester.hs

install: hrun.exe
	cp hrun.exe $RBIN