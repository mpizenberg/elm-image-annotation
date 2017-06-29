const express = require( 'express' )
const app = express()
const compression = require( 'compression' )


// return home page (client elm application)
app.use( compression() )
app.use( '/public', express.static( __dirname + '/public' ) )
app.get( '/', ( req, res ) => res.sendFile( __dirname + '/index.html' ) )


// Start the server
const port = 8001
app.listen( port, 'localhost', () => {
	console.log( "Listening on port " + port.toString() )
})
