'use strict'
/* eslint-disable no-console */

const pipe = require('it-pipe')
const lp = require('it-length-prefixed')


function toStream(stream, callback) {
  // Read utf-8 from stdin
  process.stdin.setEncoding('utf8')
  pipe(
    // Read from stdin (the source)
    process.stdin,
    //() => change,
    // Encode with length prefix (so receiving side knows how much data is coming)
    lp.encode(),
    // Write to the stream (the sink)
    stream.sink
  )
}

async function fromStream(stream) {
  let rsult = await pipe(
    // Read from the stream (the source)
    stream.source,
    // Decode length-prefixed data
    lp.decode(),
    // Sink function
    async function (source) {
      // For each chunk of data
      for await (const msg of source) {
        // Output the data as a utf8 string
        console.log('> ' + msg.toString().replace('\n', ''))
      }
    }
  )

}

module.exports = {
  toStream,
  fromStream
}