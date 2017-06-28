app.ports.askViewerSize.subscribe( () => {
	window.requestAnimationFrame( () => {
		askClientSize ( "viewer", app.ports.viewerSize )
	})
})

function askClientSize( tagId, outPort ) {
	const tag = document.getElementById(tagId)
	outPort.send([tag.clientWidth, tag.clientHeight])
}
