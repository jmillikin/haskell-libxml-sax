PATH="$PATH:$PWD/cabal-dev/bin/"

VERSION=$(awk '/^version:/{print $2}' libxml-sax.cabal)

CABAL_DEV=$(which cabal-dev)
ANANSI=$(which anansi)
XELATEX=$(which xelatex)
XZ=$(which xz)

require_cabal_dev()
{
	if [ -z "$CABAL_DEV" ]; then
		echo "Can't find 'cabal-dev' executable; make sure it exists on your "'$PATH'
		echo "Cowardly refusing to fuck with the global package database"
		exit 1
	fi
}

require_anansi()
{
	if [ -z "$ANANSI" ]; then
		echo "Can't find 'anansi' executable; running '$CABAL_DEV install anansi'"
		require_cabal_dev
		$CABAL_DEV install anansi &> /dev/null
		if [ "$?" -ne "0" ]; then
			echo "Installation failed; please install Anansi manually somehow"
			exit 1
		fi
		ANANSI=$(which anansi)
		echo "Success; anansi = $ANANSI"
	fi
}

require_xelatex()
{
	if [ -z "$XELATEX" ]; then
		echo "Can't find 'xelatex' executable; make sure it exists on your "'$PATH'
		exit 1
	fi
}

make_pdf()
{
	require_anansi
	require_xelatex
	
	rm -f *.{aux,tex,idx,log,out,toc,pdf}
	$ANANSI -w -l latex-noweb -o libxml-sax.tex libxml-sax.anansi || exit 1
	$XELATEX libxml-sax.tex > /dev/null || exit 1
	$XELATEX libxml-sax.tex > /dev/null || exit 1
	rm -f *.{aux,tex,idx,log,out,toc}
	mv libxml-sax.pdf "libxml-sax_$VERSION.pdf"
}

clean_dev_install()
{
	# require_anansi
	require_cabal_dev
	
	rm -rf hs dist
	# $ANANSI -o hs libxml-sax.anansi || exit 1
	$CABAL_DEV install || exit 1
}
