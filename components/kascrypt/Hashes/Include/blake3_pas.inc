
procedure g(state: pcuint32; a, b, c, d: csize_t; x, y: cuint32); inline;
begin
  state[a] := state[a] + state[b] + x;
  state[d] := RorDWord(state[d] xor state[a], 16);
  state[c] := state[c] + state[d];
  state[b] := RorDWord(state[b] xor state[c], 12);
  state[a] := state[a] + state[b] + y;
  state[d] := RorDWord(state[d] xor state[a], 8);
  state[c] := state[c] + state[d];
  state[b] := RorDWord(state[b] xor state[c], 7);
end;

procedure round_fn(state: pcuint32; const msg: pcuint32; round: csize_t); inline;
var
  schedule: pcuint8;
begin
  // Select the message schedule based on the round.
  schedule := MSG_SCHEDULE[round];

  // Mix the columns.
  g(state, 0, 4, 8, 12, msg[schedule[0]], msg[schedule[1]]);
  g(state, 1, 5, 9, 13, msg[schedule[2]], msg[schedule[3]]);
  g(state, 2, 6, 10, 14, msg[schedule[4]], msg[schedule[5]]);
  g(state, 3, 7, 11, 15, msg[schedule[6]], msg[schedule[7]]);

  // Mix the rows.
  g(state, 0, 5, 10, 15, msg[schedule[8]], msg[schedule[9]]);
  g(state, 1, 6, 11, 12, msg[schedule[10]], msg[schedule[11]]);
  g(state, 2, 7, 8, 13, msg[schedule[12]], msg[schedule[13]]);
  g(state, 3, 4, 9, 14, msg[schedule[14]], msg[schedule[15]]);
end;

procedure compress_pre(state: pcuint32; const cv: pcuint32;
                       const block: pcuint8; block_len: cuint8;
                       counter: cuint64; flags: cuint8); inline;
var
  block_words: array[0..15] of cuint32;
begin
  block_words[0] := load32(block + 4 * 0);
  block_words[1] := load32(block + 4 * 1);
  block_words[2] := load32(block + 4 * 2);
  block_words[3] := load32(block + 4 * 3);
  block_words[4] := load32(block + 4 * 4);
  block_words[5] := load32(block + 4 * 5);
  block_words[6] := load32(block + 4 * 6);
  block_words[7] := load32(block + 4 * 7);
  block_words[8] := load32(block + 4 * 8);
  block_words[9] := load32(block + 4 * 9);
  block_words[10] := load32(block + 4 * 10);
  block_words[11] := load32(block + 4 * 11);
  block_words[12] := load32(block + 4 * 12);
  block_words[13] := load32(block + 4 * 13);
  block_words[14] := load32(block + 4 * 14);
  block_words[15] := load32(block + 4 * 15);

  state[0] := cv[0];
  state[1] := cv[1];
  state[2] := cv[2];
  state[3] := cv[3];
  state[4] := cv[4];
  state[5] := cv[5];
  state[6] := cv[6];
  state[7] := cv[7];
  state[8] := BLAKE3_IV[0];
  state[9] := BLAKE3_IV[1];
  state[10] := BLAKE3_IV[2];
  state[11] := BLAKE3_IV[3];
  state[12] := Int64Rec(counter).Lo;
  state[13] := Int64Rec(counter).Hi;
  state[14] := cuint32(block_len);
  state[15] := cuint32(flags);

  round_fn(state, @block_words[0], 0);
  round_fn(state, @block_words[0], 1);
  round_fn(state, @block_words[0], 2);
  round_fn(state, @block_words[0], 3);
  round_fn(state, @block_words[0], 4);
  round_fn(state, @block_words[0], 5);
  round_fn(state, @block_words[0], 6);
end;

procedure blake3_compress_in_place_portable(cv: pcuint32;
                                       const block: pcuint8;
                                       block_len: cuint8; counter: cuint64;
                                       flags: cuint8);
var
  state: array[0..15] of cuint32;
begin
  compress_pre(state, cv, block, block_len, counter, flags);
  cv[0] := state[0] xor state[8];
  cv[1] := state[1] xor state[9];
  cv[2] := state[2] xor state[10];
  cv[3] := state[3] xor state[11];
  cv[4] := state[4] xor state[12];
  cv[5] := state[5] xor state[13];
  cv[6] := state[6] xor state[14];
  cv[7] := state[7] xor state[15];
end;

procedure blake3_compress_xof_portable(const cv: pcuint32;
                                       const block: pcuint8;
                                       block_len: cuint8; counter: cuint64;
                                       flags: cuint8; out_: pcuint8);
var
  state: array[0..15] of cuint32;
begin
  compress_pre(state, cv, block, block_len, counter, flags);

  store32(@out_[0 * 4], state[0] xor state[8]);
  store32(@out_[1 * 4], state[1] xor state[9]);
  store32(@out_[2 * 4], state[2] xor state[10]);
  store32(@out_[3 * 4], state[3] xor state[11]);
  store32(@out_[4 * 4], state[4] xor state[12]);
  store32(@out_[5 * 4], state[5] xor state[13]);
  store32(@out_[6 * 4], state[6] xor state[14]);
  store32(@out_[7 * 4], state[7] xor state[15]);
  store32(@out_[8 * 4], state[8] xor cv[0]);
  store32(@out_[9 * 4], state[9] xor cv[1]);
  store32(@out_[10 * 4], state[10] xor cv[2]);
  store32(@out_[11 * 4], state[11] xor cv[3]);
  store32(@out_[12 * 4], state[12] xor cv[4]);
  store32(@out_[13 * 4], state[13] xor cv[5]);
  store32(@out_[14 * 4], state[14] xor cv[6]);
  store32(@out_[15 * 4], state[15] xor cv[7]);
end;

procedure hash_one_portable(input: pcuint8; blocks: csize_t;
                              const key: pcuint32; counter: cuint64;
                              flags: uint8; flags_start: cuint8;
                              flags_end: cuint8; out_: pcuint8); inline;
var
  block_flags: cuint8;
  cv: array[0..7] of cuint32;
begin
  Move(key^, cv[0], BLAKE3_KEY_LEN);
  block_flags := flags or flags_start;
  while (blocks > 0) do
  begin
    if (blocks = 1) then begin
      block_flags := block_flags or flags_end;
    end;
    blake3_compress_in_place_portable(cv, input, BLAKE3_BLOCK_LEN, counter,
                                      block_flags);
    input := @input[BLAKE3_BLOCK_LEN];
    blocks -= 1;
    block_flags := flags;
  end;
  store_cv_words(out_, cv);
end;

procedure blake3_hash_many_portable(inputs: ppcuint8; num_inputs: csize_t;
                                    blocks: csize_t; const key: pcuint32;
                                    counter: cuint64; increment_counter: boolean32;
                                    flags: cuint8; flags_start: cuint8;
                                    flags_end: cuint8; out_: pcuint8);
begin
  while (num_inputs > 0) do
  begin
    hash_one_portable(inputs[0], blocks, key, counter, flags, flags_start,
                      flags_end, out_);
    if (increment_counter) then begin
      counter += 1;
    end;
    inputs += 1;
    num_inputs -= 1;
    out_ := @out_[BLAKE3_OUT_LEN];
  end;
end;

