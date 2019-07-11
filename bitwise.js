goog.provide('bitwise');

// var r, g, b;

// r = 255

// g = 184

// b = 76

// r.toString(2)
// "11111111"
// g.toString(2)
// "10111000"
// b.toString(2)
// "1001100"
// "01001100"


// 110
// 101
// 110
// 110
// 111
// 101
// 100
// 100


// 110101110110111101100100

// 1
// 1
// 1
// 1
// 1
// 1
// 1
// 1

// 1
// 0
// 1
// 1
// 1
// 0
// 0
// 0


// 0
// 1
// 0
// 0
// 1
// 1
// 0
// 0
// concatted: 111111111011100001001100

// interleaved: 110101110110111101100100

bitwise.getBit = function(x, i) {
    return (x & (1 << i)) != 0;
}

bitwise.interleaveBytes = function(r, g, b) {
    var result = 0;

    for (var i = 7; i >= 0; i--) {
        result <<= 1;
        result |= bitwise.getBit(r, i);
        result <<= 1;
        result |= bitwise.getBit(g, i);
        result <<= 1;
        result |= bitwise.getBit(b, i);
    }
    // console.log(r.toString(2), g.toString(2), b.toString(2), result.toString(2));

    return result;
}

bitwise.interleaveRGBA = function(rgba) {
    var result = 0;

    for (var i = 0; i < 8; i++) {
        result <<= 1;
        result |= bitwise.getBit(rgba, 31 - i); //r
        result <<= 1;
        result |= bitwise.getBit(rgba, 23 - i); //g
        result <<= 1;
        result |= bitwise.getBit(rgba, 15 - i); //b
    }

    // console.log(rgba, rgba.toString(2), result, result.toString(2));

    return result;
}

bitwise.deInterleavePCM = function(pcm24) {
    // console.log(pcm24, pcm24.toString(2));
    var r,g,b = 0;
    var a = 255;

    for (var i = 0; i < 24; i+=3) {
        r <<= 1;
        r |= bitwise.getBit(pcm24, 23 - i);
        g <<= 1;
        g |= bitwise.getBit(pcm24, 22 - i);
        b <<= 1;
        b |= bitwise.getBit(pcm24, 21 - i);
    }

    var resulta = new Uint8ClampedArray([a, b, g, r]); // wtf?
    var resultb = new Uint32Array(resulta.buffer);

    // console.log(r, g, b, a, resulta, resultb, resultb[0], resultb[0].toString(2));
    // console.log(resultb[0]);
    return resultb[0];
}