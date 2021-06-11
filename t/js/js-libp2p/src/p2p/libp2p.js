'use strict'

const TCP = require('libp2p-tcp');
const WS = require('libp2p-websockets');
const mplex = require('libp2p-mplex');
const { NOISE } = require('libp2p-noise');
const defaultsDeep = require('@nodeutils/defaults-deep');
const libp2p = require('libp2p');

async function createLibp2p(_options) {
  const defaults = {
    modules: {
      transport: [TCP, WS],
      streamMuxer: [mplex],
      connEncryption: [NOISE],
    },
  }

  return libp2p.create(defaultsDeep(_options, defaults));
}

module.exports = createLibp2p;