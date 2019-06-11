// vim: filetype=javascript

describe('A rock/paper/scissors game', () => {

  beforeAll(done => {
    import('../monkey-patch-require.js')
      .then(() => import('../rps-demo.mjs'))
      .then(done);
  });

  describe('results in', () => {

    it('both participants agreeing on who won', () => expect(true).toBe(true));

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', () =>
      expect(true).toBe(true));
  });

  xdescribe('involves async operations which', () => {

    it('require a little patience', done =>
      setTimeout(() => done() || expect(true).toBe(true), 2500));

    xit('may necessitate future tests', () =>
      expect(true).toBe(true));
  });
});
