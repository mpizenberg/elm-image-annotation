app.ports.askViewerSize.subscribe( () => {
	window.requestAnimationFrame( () => {
		askClientSize ( "viewer", app.ports.viewerSize )
	})
})


app.ports.exportAnnotations.subscribe( (strAnnotations) => {
	download( strAnnotations, "annotations.json", "text/plain" )
})


function askClientSize( tagId, outPort ) {
	const tag = document.getElementById(tagId)
	outPort.send([tag.clientWidth, tag.clientHeight])
}


// Download a file through an href temporary DOM element. see ref at:
// http://stackoverflow.com/a/30832210/4822734
// and FileSaver inspiration for click:
// https://github.com/eligrey/FileSaver.js/blob/4ac2c6d6c286ea5e57e4098c58b33bfe9ec26f05/FileSaver.js#L29
// example use:
// download( str, "selection.json", "text/plain" )
function download( data, name, data_type ) {
	const a = document.createElement("a")
	const data_file = new Blob( [data], {type: data_type} )
	a.addEventListener( 'click', (event) => {
		a.href = window.URL.createObjectURL( data_file )
		a.download = name
	})
	const click = (node) => {
		var event = new MouseEvent( "click" )
		node.dispatchEvent( event )
	}
	click( a )
}

