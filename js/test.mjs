import * as Stdlib from './stdlib.mjs';

import test from 'ava';

test('uint256_to_bytes', t => {
	t.deepEqual(Stdlib.uint256_to_bytes(99), "0000000000000000000000000000000000000000000000000000000000000063");
});
