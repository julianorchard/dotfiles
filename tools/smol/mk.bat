@echo off

for %%A in (%*) do (

	mkdir "%%A"
	cd %%A

)