{
   BLAKE3 - cryptographic hash function.

   The C code is copyright Samuel Neves and Jack O'Connor, 2019-2020.
   The assembly code is copyright Samuel Neves, 2019-2020.
   The Pascal translation by Alexander Koblov, 2020.

   This work is released into the public domain with CC0 1.0.
   Alternatively, it is licensed under the Apache License 2.0.
}

unit DCblake3;

{$mode objfpc}{$H+}
{$inline on}{$Q-}
{$macro on}{$R-}

interface

uses
  Classes, SysUtils, CTypes;

const
  BLAKE3_KEY_LEN = 32;
  BLAKE3_OUT_LEN = 32;
  BLAKE3_BLOCK_LEN = 64;
  BLAKE3_CHUNK_LEN = 1024;
  BLAKE3_MAX_DEPTH = 54;
  BLAKE3_MAX_SIMD_DEGREE = 16;

{$if defined(CPUX86_64)}
  MAX_SIMD_DEGREE = 16;
{$else}
  MAX_SIMD_DEGREE = 1;
{$endif}

{$if (MAX_SIMD_DEGREE > 2)}
  MAX_SIMD_DEGREE_OR_2 = MAX_SIMD_DEGREE;
{$else}
  MAX_SIMD_DEGREE_OR_2 = 2;
{$endif}

const BLAKE3_IV: array[0..7] of cuint32 = (
  $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
  $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );

const MSG_SCHEDULE: array[0..6] of array[0..15] of cuint8 = (
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    (2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8),
    (3, 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1),
    (10, 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6),
    (12, 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4),
    (9, 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7),
    (11, 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13)
);

type
  ppcuint8 = ^pcuint8;

  Tblake_cv = array[0..7] of cuint32;

  Pblake3_chunk_state = ^blake3_chunk_state;
  blake3_chunk_state = record
    cv: array[0..7] of cuint32;
    chunk_counter: cuint64;
    buf: array[0..Pred(BLAKE3_BLOCK_LEN)] of cuint8;
    buf_len: cuint8;
    blocks_compressed: cuint8;
    flags: cuint8;
  end;

  Pblake3_hasher = ^blake3_hasher;
  blake3_hasher = record
    key: array[0..7] of cuint32;
    chunk: blake3_chunk_state;
    cv_stack_len: cuint8;
    cv_stack: array[0..Pred((BLAKE3_MAX_DEPTH + 1) * BLAKE3_OUT_LEN)] of cuint8;
  end;

procedure blake3_hasher_init(self: Pblake3_hasher);
procedure blake3_hasher_update(self: Pblake3_hasher; const input: Pointer; input_len: csize_t);
procedure blake3_hasher_finalize(const self: Pblake3_hasher; out_: pcuint8; out_len: csize_t);

implementation

{$IF DEFINED(CPUX86_64)}
uses
  CPU;
{$ENDIF}

type
  blake3_flags = (
    CHUNK_START         = 1 shl 0,
    CHUNK_END           = 1 shl 1,
    PARENT              = 1 shl 2,
    ROOT                = 1 shl 3,
    KEYED_HASH          = 1 shl 4,
    DERIVE_KEY_CONTEXT  = 1 shl 5,
    DERIVE_KEY_MATERIAL = 1 shl 6
  );

  Poutput_t = ^output_t;
  output_t = record
    input_cv: array[0..7] of cuint32;
    counter: cuint64;
    block: array[0..Pred(BLAKE3_BLOCK_LEN)] of cuint8;
    block_len: cuint8;
    flags: cuint8;
  end;

function load32( const src: Pointer ): cuint32; inline;
begin
  Result := NtoLE(pcuint32(src)^);
end;

procedure store32( dst: pointer; w: cuint32 ); inline;
begin
  pcuint32(dst)^ := LEtoN(w);
end;

procedure store_cv_words(bytes_out: pcuint8; cv_words: pcuint32); inline;
begin
  store32(@bytes_out[0 * 4], cv_words[0]);
  store32(@bytes_out[1 * 4], cv_words[1]);
  store32(@bytes_out[2 * 4], cv_words[2]);
  store32(@bytes_out[3 * 4], cv_words[3]);
  store32(@bytes_out[4 * 4], cv_words[4]);
  store32(@bytes_out[5 * 4], cv_words[5]);
  store32(@bytes_out[6 * 4], cv_words[6]);
  store32(@bytes_out[7 * 4], cv_words[7]);
end;

function round_down_to_power_of_2(x: cuint64): cuint64; inline;
begin
  Result := cuint64(1) shl BsrQWord(x or 1);
end;

procedure chunk_state_init(self: Pblake3_chunk_state; const key: pcuint32;
                           flags: cuint8); inline;
begin
  Move(key^, self^.cv[0], BLAKE3_KEY_LEN);
  self^.chunk_counter := 0;
  FillChar(self^.buf[0], BLAKE3_BLOCK_LEN, 0);
  self^.buf_len := 0;
  self^.blocks_compressed := 0;
  self^.flags := flags;
end;

procedure chunk_state_reset(self: Pblake3_chunk_state; const key: pcuint32;
                            chunk_counter: cuint64); inline;
begin
  Move(key^, self^.cv[0], BLAKE3_KEY_LEN);
  self^.chunk_counter := chunk_counter;
  self^.blocks_compressed := 0;
  FillChar(self^.buf, BLAKE3_BLOCK_LEN, 0);
  self^.buf_len := 0;
end;

function chunk_state_len(const self: Pblake3_chunk_state): csize_t; inline;
begin
  Result := (BLAKE3_BLOCK_LEN * csize_t(self^.blocks_compressed)) + (csize_t(self^.buf_len));
end;

function chunk_state_fill_buf(self: Pblake3_chunk_state;
                                   const input: pcuint8; input_len: csize_t): csize_t; inline;
var
  dest: pcuint8;
begin
  Result := BLAKE3_BLOCK_LEN - (csize_t(self^.buf_len));
  if (Result > input_len) then begin
    Result := input_len;
  end;
  dest := PByte(self^.buf) + (csize_t(self^.buf_len));
  Move(input^, dest^, Result);
  self^.buf_len += cuint8(Result);
end;

function chunk_state_maybe_start_flag(const self: Pblake3_chunk_state): cuint8; inline;
begin
  if (self^.blocks_compressed = 0) then
    Result := cuint8(CHUNK_START)
   else begin
    Result := 0;
  end;
end;

function make_output(const input_cv: Tblake_cv; const block: pcuint8;
                     block_len: cuint8; counter: cuint64; flags: cuint8): output_t; inline;
begin
  Move(input_cv[0], Result.input_cv[0], 32);
  Move(block^, Result.block[0], BLAKE3_BLOCK_LEN);
  Result.block_len := block_len;
  Result.counter := counter;
  Result.flags := flags;
end;

{$IF DEFINED(CPUX86_64)}
  {$include blake3_sse2.inc}
  {$include blake3_sse41.inc}
  {$include blake3_avx2.inc}
{$ELSE}
  {$include blake3_pas.inc}
{$ENDIF}

var
  blake3_simd_degree: csize_t; // The dynamically detected SIMD degree of the current platform

  blake3_compress_in_place: procedure(cv: pcuint32;
                                      const block: pcuint8;
                                      block_len: cuint8; counter: cuint64;
                                      flags: cuint8);

  blake3_compress_xof: procedure(const cv: pcuint32;
                                 const block: pcuint8;
                                 block_len: cuint8; counter: cuint64;
                                 flags: cuint8; out_: pcuint8);

  blake3_hash_many: procedure(inputs: ppcuint8; num_inputs: csize_t;
                              blocks: csize_t; const key: pcuint32;
                              counter: cuint64; increment_counter: boolean32;
                              flags: cuint8; flags_start: cuint8;
                              flags_end: cuint8; out_: pcuint8);

procedure output_chaining_value(const self: Poutput_t; cv: pcuint8); inline;
var
  cv_words: Tblake_cv;
begin
  Move(self^.input_cv[0], cv_words[0], 32);
  blake3_compress_in_place(cv_words, self^.block, self^.block_len,
                           self^.counter, self^.flags);
  store_cv_words(cv, cv_words);
end;

procedure output_root_bytes(const self: Poutput_t; seek: cuint64; out_: pcuint8;
                            out_len: csize_t); inline;
var
  memcpy_len: csize_t;
  available_bytes: csize_t;
  offset_within_block: csize_t;
  output_block_counter: cuint64;
  wide_buf: array[0..63] of cuint8;
begin
  output_block_counter := seek div 64;
  offset_within_block := seek mod 64;
  while (out_len > 0) do
  begin
    blake3_compress_xof(self^.input_cv, self^.block, self^.block_len,
                        output_block_counter, self^.flags or cuint8(ROOT), wide_buf);
    available_bytes := 64 - offset_within_block;

    if (out_len > available_bytes) then
      memcpy_len := available_bytes
    else begin
      memcpy_len := out_len;
    end;
    Move(wide_buf[offset_within_block], out_^, memcpy_len);
    out_ += memcpy_len;
    out_len -= memcpy_len;
    output_block_counter += 1;
    offset_within_block := 0;
  end;
end;

procedure chunk_state_update(self: Pblake3_chunk_state; input: pcuint8;
                             input_len: csize_t); inline;
var
  take: csize_t;
begin
  if (self^.buf_len > 0) then
  begin
    take := chunk_state_fill_buf(self, input, input_len);
    input += take;
    input_len -= take;
    if (input_len > 0) then
    begin
      blake3_compress_in_place(
          self^.cv, self^.buf, BLAKE3_BLOCK_LEN, self^.chunk_counter,
          self^.flags or chunk_state_maybe_start_flag(self));
      self^.blocks_compressed += 1;
      self^.buf_len := 0;
      FillChar(self^.buf[0], BLAKE3_BLOCK_LEN, 0);
    end;
  end;

  while (input_len > BLAKE3_BLOCK_LEN) do
  begin
    blake3_compress_in_place(self^.cv, input, BLAKE3_BLOCK_LEN,
                             self^.chunk_counter,
                             self^.flags or chunk_state_maybe_start_flag(self));
    self^.blocks_compressed += 1;
    input += BLAKE3_BLOCK_LEN;
    input_len -= BLAKE3_BLOCK_LEN;
  end;

  take := chunk_state_fill_buf(self, input, input_len);
  input += take;
  input_len -= take;
end;

function chunk_state_output(const self: Pblake3_chunk_state): output_t; inline;
var
  block_flags: cuint8;
begin
  block_flags := self^.flags or chunk_state_maybe_start_flag(self) or cuint8(CHUNK_END);
  Result := make_output(self^.cv, self^.buf, self^.buf_len, self^.chunk_counter, block_flags);
end;

function parent_output(const block: pcuint8; const key: pcuint32; flags: cuint8): output_t; inline;
begin
  Result := make_output(key, block, BLAKE3_BLOCK_LEN, 0, flags or cuint8(PARENT));
end;

function left_len(content_len: csize_t): csize_t; inline;
var
  full_chunks: csize_t;
begin
  full_chunks := (content_len - 1) div BLAKE3_CHUNK_LEN;
  Result := round_down_to_power_of_2(full_chunks) * BLAKE3_CHUNK_LEN;
end;

function compress_chunks_parallel(const input: pcuint8; input_len: csize_t;
                                       const key: pcuint32;
                                       chunk_counter: cuint64; flags: cuint8;
                                       out_: pcuint8): csize_t; inline;
var
  counter: cuint64;
  output: output_t;
  input_position: csize_t = 0;
  chunks_array_len: csize_t = 0;
  chunk_state: blake3_chunk_state;
  chunks_array: array[0..Pred(MAX_SIMD_DEGREE)] of pcuint8;
begin
  assert(0 < input_len);
  assert(input_len <= MAX_SIMD_DEGREE * BLAKE3_CHUNK_LEN);

  while (input_len - input_position >= BLAKE3_CHUNK_LEN) do
  begin
    chunks_array[chunks_array_len] := @input[input_position];
    input_position += BLAKE3_CHUNK_LEN;
    chunks_array_len += 1;
  end;

  blake3_hash_many(chunks_array, chunks_array_len,
                   BLAKE3_CHUNK_LEN div BLAKE3_BLOCK_LEN, key, chunk_counter,
                   true, flags, cuint8(CHUNK_START), cuint8(CHUNK_END), out_);

  // Hash the remaining partial chunk, if there is one. Note that the empty
  // chunk (meaning the empty message) is a different codepath.
  if (input_len > input_position) then
  begin
    counter := chunk_counter + cuint64(chunks_array_len);
    chunk_state_init(@chunk_state, key, flags);
    chunk_state.chunk_counter := counter;
    chunk_state_update(@chunk_state, @input[input_position],
                       input_len - input_position);
    output := chunk_state_output(@chunk_state);
    output_chaining_value(@output, @out_[chunks_array_len * BLAKE3_OUT_LEN]);
    Result := chunks_array_len + 1;
  end
  else begin
    Result := chunks_array_len;
  end;
end;

function compress_parents_parallel(const child_chaining_values: pcuint8;
                                        num_chaining_values: csize_t;
                                        const key: pcuint32; flags: cuint8;
                                        out_: pcuint8): csize_t; inline;
var
 parents_array_len: csize_t = 0;
 parents_array: array[0..Pred(MAX_SIMD_DEGREE_OR_2)] of puint8;
begin
  assert(2 <= num_chaining_values);
  assert(num_chaining_values <= 2 * MAX_SIMD_DEGREE_OR_2);


  while (num_chaining_values - (2 * parents_array_len) >= 2) do
  begin
    parents_array[parents_array_len] :=
        @child_chaining_values[2 * parents_array_len * BLAKE3_OUT_LEN];
    parents_array_len += 1;
  end;

  blake3_hash_many(parents_array, parents_array_len, 1, key,
                   0, // Parents always use counter 0.
                   false, flags or cuint8(PARENT),
                   0, // Parents have no start flags.
                   0, // Parents have no end flags.
                   out_);

  // If there's an odd child left over, it becomes an output.
  if (num_chaining_values > 2 * parents_array_len) then
  begin
    Move(child_chaining_values[2 * parents_array_len * BLAKE3_OUT_LEN],
         out_[parents_array_len * BLAKE3_OUT_LEN], BLAKE3_OUT_LEN);
    Result := parents_array_len + 1;
  end
  else begin
    Result := parents_array_len;
  end;
end;

function blake3_compress_subtree_wide(const input: pcuint8;
                                      input_len: csize_t;
                                      const key: pcuint32;
                                      chunk_counter: cuint64;
                                      flags: cuint8; out_: pcuint8): csize_t;
var
 left_n: csize_t;
 degree: csize_t;
 right_n: csize_t;
 right_cvs: pcuint8;
 right_input: pcuint8;
 left_input_len: csize_t;
 right_input_len: csize_t;
 right_chunk_counter: cuint64;
 num_chaining_values: csize_t;
 cv_array: array[0..Pred(2 * MAX_SIMD_DEGREE_OR_2 * BLAKE3_OUT_LEN)] of cuint8;
begin
  // Note that the single chunk case does *not* bump the SIMD degree up to 2
  // when it is 1. If this implementation adds multi-threading in the future,
  // this gives us the option of multi-threading even the 2-chunk case, which
  // can help performance on smaller platforms.
  if (input_len <= blake3_simd_degree * BLAKE3_CHUNK_LEN) then
  begin
    Result:= compress_chunks_parallel(input, input_len, key, chunk_counter, flags, out_);
    Exit;
  end;

  // With more than simd_degree chunks, we need to recurse. Start by dividing
  // the input into left and right subtrees. (Note that this is only optimal
  // as long as the SIMD degree is a power of 2. If we ever get a SIMD degree
  // of 3 or something, we'll need a more complicated strategy.)
  left_input_len := left_len(input_len);
  right_input_len := input_len - left_input_len;
  right_input := @input[left_input_len];
  right_chunk_counter := chunk_counter + cuint64(left_input_len div BLAKE3_CHUNK_LEN);

  // Make space for the child outputs. Here we use MAX_SIMD_DEGREE_OR_2 to
  // account for the special case of returning 2 outputs when the SIMD degree
  // is 1.
  degree := blake3_simd_degree;
  if (left_input_len > BLAKE3_CHUNK_LEN) and (degree = 1)  then
  begin
    // The special case: We always use a degree of at least two, to make
    // sure there are two outputs. Except, as noted above, at the chunk
    // level, where we allow degree=1. (Note that the 1-chunk-input case is
    // a different codepath.)
    degree := 2;
  end;
  right_cvs := @cv_array[degree * BLAKE3_OUT_LEN];

  // Recurse! If this implementation adds multi-threading support in the
  // future, this is where it will go.
  left_n := blake3_compress_subtree_wide(input, left_input_len, key,
                                               chunk_counter, flags, cv_array);
  right_n := blake3_compress_subtree_wide(
      right_input, right_input_len, key, right_chunk_counter, flags, right_cvs);

  // The special case again. If simd_degree=1, then we'll have left_n=1 and
  // right_n=1. Rather than compressing them into a single output, return
  // them directly, to make sure we always have at least two outputs.
  if (left_n = 1) then
  begin
    Move(cv_array[0], out_^, 2 * BLAKE3_OUT_LEN);
    Exit(2);
  end;

  // Otherwise, do one layer of parent node compression.
  num_chaining_values := left_n + right_n;
  Result := compress_parents_parallel(cv_array, num_chaining_values, key, flags, out_);
end;

procedure compress_subtree_to_parent_node(
    const input: pcuint8; input_len: csize_t; const key: pcuint32;
    chunk_counter: cuint64; flags: cuint8; out_: pcuint8); inline;
var
  num_cvs: csize_t;
  cv_array: array[0..Pred(MAX_SIMD_DEGREE_OR_2 * BLAKE3_OUT_LEN)] of cuint8;
  out_array: array[0..Pred(MAX_SIMD_DEGREE_OR_2 * BLAKE3_OUT_LEN div 2)] of cuint8;
begin
  assert(input_len > BLAKE3_CHUNK_LEN);

  num_cvs := blake3_compress_subtree_wide(input, input_len, key,
                                                chunk_counter, flags, cv_array);

  // If MAX_SIMD_DEGREE is greater than 2 and there's enough input,
  // compress_subtree_wide() returns more than 2 chaining values. Condense
  // them into 2 by forming parent nodes repeatedly.
  while (num_cvs > 2) do
  begin
    num_cvs :=
        compress_parents_parallel(cv_array, num_cvs, key, flags, out_array);
    Move(out_array[0], cv_array[0], num_cvs * BLAKE3_OUT_LEN);
  end;
  Move(cv_array[0], out_^, 2 * BLAKE3_OUT_LEN);
end;

procedure hasher_init_base(self: Pblake3_hasher; const key: pcuint32;
                             flags: cuint8); inline;
begin
  Move(key^, self^.key[0], BLAKE3_KEY_LEN);
  chunk_state_init(@self^.chunk, key, flags);
  self^.cv_stack_len := 0;
end;

procedure blake3_hasher_init(self: Pblake3_hasher); inline;
begin
  hasher_init_base(self, BLAKE3_IV, 0);
end;

procedure hasher_merge_cv_stack(self: Pblake3_hasher; total_len: cuint64); inline;
var
  output: output_t;
  parent_node: pcuint8;
  post_merge_stack_len: csize_t;
begin
  post_merge_stack_len := csize_t(popcnt(total_len));
  while (self^.cv_stack_len > post_merge_stack_len) do
  begin
    parent_node := @self^.cv_stack[(self^.cv_stack_len - 2) * BLAKE3_OUT_LEN];
    output := parent_output(parent_node, self^.key, self^.chunk.flags);
    output_chaining_value(@output, parent_node);
    self^.cv_stack_len -= 1;
  end;
end;

procedure hasher_push_cv(self: Pblake3_hasher; new_cv: pcuint8;
                           chunk_counter: cuint64); inline;
begin
  hasher_merge_cv_stack(self, chunk_counter);
  Move(new_cv^, self^.cv_stack[self^.cv_stack_len * BLAKE3_OUT_LEN], BLAKE3_OUT_LEN);
  self^.cv_stack_len += 1;
end;

procedure blake3_hasher_update(self: Pblake3_hasher; const input: Pointer;
                               input_len: csize_t);
var
  take: csize_t;
  output: output_t;
  subtree_len: csize_t;
  input_bytes: pcuint8;
  count_so_far: cuint64;
  subtree_chunks: cuint64;
  chunk_state: blake3_chunk_state;
  chunk_cv: array[0..31] of cuint8;
  cv: array[0..Pred(BLAKE3_OUT_LEN)] of cuint8;
  cv_pair: array[0..Pred(2 * BLAKE3_OUT_LEN)] of cuint8;
begin
  // Explicitly checking for zero avoids causing UB by passing a null pointer
  // to memcpy. This comes up in practice with things like:
  //   std::vector<uint8_t> v;
  //   blake3_hasher_update(&hasher, v.data(), v.size());
  if (input_len = 0) then Exit;

  input_bytes := pcuint8(input);

  // If we have some partial chunk bytes in the internal chunk_state, we need
  // to finish that chunk first.
  if (chunk_state_len(@self^.chunk) > 0)  then
  begin
    take := BLAKE3_CHUNK_LEN - chunk_state_len(@self^.chunk);
    if (take > input_len) then begin
      take := input_len;
    end;
    chunk_state_update(@self^.chunk, input_bytes, take);
    input_bytes += take;
    input_len -= take;
    // If we've filled the current chunk and there's more coming, finalize this
    // chunk and proceed. In this case we know it's not the root.
    if (input_len > 0) then
    begin
      output := chunk_state_output(@self^.chunk);
      output_chaining_value(@output, chunk_cv);
      hasher_push_cv(self, chunk_cv, self^.chunk.chunk_counter);
      chunk_state_reset(@self^.chunk, self^.key, self^.chunk.chunk_counter + 1);
    end
    else begin
      Exit;
    end;
  end;

  // Now the chunk_state is clear, and we have more input. If there's more than
  // a single chunk (so, definitely not the root chunk), hash the largest whole
  // subtree we can, with the full benefits of SIMD (and maybe in the future,
  // multi-threading) parallelism. Two restrictions:
  // - The subtree has to be a power-of-2 number of chunks. Only subtrees along
  //   the right edge can be incomplete, and we don't know where the right edge
  //   is going to be until we get to finalize().
  // - The subtree must evenly divide the total number of chunks up until this
  //   point (if total is not 0). If the current incomplete subtree is only
  //   waiting for 1 more chunk, we can't hash a subtree of 4 chunks. We have
  //   to complete the current subtree first.
  // Because we might need to break up the input to form powers of 2, or to
  // evenly divide what we already have, this part runs in a loop.
  while (input_len > BLAKE3_CHUNK_LEN) do
  begin
    subtree_len := round_down_to_power_of_2(input_len);
    count_so_far := self^.chunk.chunk_counter * BLAKE3_CHUNK_LEN;
    // Shrink the subtree_len until it evenly divides the count so far. We know
    // that subtree_len itself is a power of 2, so we can use a bitmasking
    // trick instead of an actual remainder operation. (Note that if the caller
    // consistently passes power-of-2 inputs of the same size, as is hopefully
    // typical, this loop condition will always fail, and subtree_len will
    // always be the full length of the input.)
    //
    // An aside: We don't have to shrink subtree_len quite this much. For
    // example, if count_so_far is 1, we could pass 2 chunks to
    // compress_subtree_to_parent_node. Since we'll get 2 CVs back, we'll still
    // get the right answer in the end, and we might get to use 2-way SIMD
    // parallelism. The problem with this optimization, is that it gets us
    // stuck always hashing 2 chunks. The total number of chunks will remain
    // odd, and we'll never graduate to higher degrees of parallelism. See
    // https://github.com/BLAKE3-team/BLAKE3/issues/69.
    while (((cuint64(subtree_len - 1)) and count_so_far) <> 0) do
    begin
      subtree_len := subtree_len div 2;
    end;
    // The shrunken subtree_len might now be 1 chunk long. If so, hash that one
    // chunk by itself. Otherwise, compress the subtree into a pair of CVs.
    subtree_chunks := subtree_len div BLAKE3_CHUNK_LEN;
    if (subtree_len <= BLAKE3_CHUNK_LEN) then
    begin
      chunk_state_init(@chunk_state, self^.key, self^.chunk.flags);
      chunk_state.chunk_counter := self^.chunk.chunk_counter;
      chunk_state_update(@chunk_state, input_bytes, subtree_len);
      output := chunk_state_output(@chunk_state);
      output_chaining_value(@output, cv);
      hasher_push_cv(self, cv, chunk_state.chunk_counter);
    end
    else begin
      // This is the high-performance happy path, though getting here depends
      // on the caller giving us a long enough input.
      compress_subtree_to_parent_node(input_bytes, subtree_len, self^.key,
                                      self^.chunk.chunk_counter,
                                      self^.chunk.flags, cv_pair);
      hasher_push_cv(self, cv_pair, self^.chunk.chunk_counter);
      hasher_push_cv(self, @cv_pair[BLAKE3_OUT_LEN],
                     self^.chunk.chunk_counter + (subtree_chunks div 2));
    end;
    self^.chunk.chunk_counter += subtree_chunks;
    input_bytes += subtree_len;
    input_len -= subtree_len;
  end;

  // If there's any remaining input less than a full chunk, add it to the chunk
  // state. In that case, also do a final merge loop to make sure the subtree
  // stack doesn't contain any unmerged pairs. The remaining input means we
  // know these merges are non-root. This merge loop isn't strictly necessary
  // here, because hasher_push_chunk_cv already does its own merge loop, but it
  // simplifies blake3_hasher_finalize below.
  if (input_len > 0) then
  begin
    chunk_state_update(@self^.chunk, input_bytes, input_len);
    hasher_merge_cv_stack(self, self^.chunk.chunk_counter);
  end;
end;

procedure blake3_hasher_finalize_seek(const self: Pblake3_hasher; seek: cuint64;
                                      out_: pcuint8; out_len: csize_t);
var
  output: output_t;
  cvs_remaining: csize_t;
  parent_block: array[0..Pred(BLAKE3_BLOCK_LEN)] of cuint8;
begin
  // Explicitly checking for zero avoids causing UB by passing a null pointer
  // to memcpy. This comes up in practice with things like:
  //   std::vector<uint8_t> v;
  //   blake3_hasher_finalize(&hasher, v.data(), v.size());
  if (out_len = 0) then Exit;

  // If the subtree stack is empty, then the current chunk is the root.
  if (self^.cv_stack_len = 0) then
  begin
    output := chunk_state_output(@self^.chunk);
    output_root_bytes(@output, seek, out_, out_len);
    Exit;
  end;
  // If there are any bytes in the chunk state, finalize that chunk and do a
  // roll-up merge between that chunk hash and every subtree in the stack. In
  // this case, the extra merge loop at the end of blake3_hasher_update
  // guarantees that none of the subtrees in the stack need to be merged with
  // each other first. Otherwise, if there are no bytes in the chunk state,
  // then the top of the stack is a chunk hash, and we start the merge from
  // that.
  if (chunk_state_len(@self^.chunk) > 0) then
  begin
    cvs_remaining := self^.cv_stack_len;
    output := chunk_state_output(@self^.chunk);
  end
  else begin
    // There are always at least 2 CVs in the stack in this case.
    cvs_remaining := self^.cv_stack_len - 2;
    output := parent_output(@self^.cv_stack[cvs_remaining * 32], self^.key,
                            self^.chunk.flags);
  end;
  while (cvs_remaining > 0) do
  begin
    cvs_remaining -= 1;
    Move(self^.cv_stack[cvs_remaining * 32], parent_block[0], 32);
    output_chaining_value(@output, @parent_block[32]);
    output := parent_output(parent_block, self^.key, self^.chunk.flags);
  end;
  output_root_bytes(@output, seek, out_, out_len);
end;

procedure blake3_hasher_finalize(const self: Pblake3_hasher; out_: pcuint8; out_len: csize_t);
begin
  blake3_hasher_finalize_seek(self, 0, out_, out_len);
end;

initialization
{$IF DEFINED(CPUX86_64)}
  if AVX2Support then
  begin
    blake3_simd_degree:= 8;
    blake3_compress_in_place:= @blake3_compress_in_place_sse41;
    blake3_compress_xof:= @blake3_compress_xof_sse41;
    blake3_hash_many:= @blake3_hash_many_avx2;
  end
  else if SSE41Support then
  begin
    blake3_simd_degree:= 4;
    blake3_compress_in_place:= @blake3_compress_in_place_sse41;
    blake3_compress_xof:= @blake3_compress_xof_sse41;
    blake3_hash_many:= @blake3_hash_many_sse41;
  end
  else begin
    blake3_simd_degree:= 4;
    blake3_compress_in_place:= @blake3_compress_in_place_sse2;
    blake3_compress_xof:= @blake3_compress_xof_sse2;
    blake3_hash_many:= @blake3_hash_many_sse2;
  end;
{$ELSE}
  blake3_simd_degree:= 1;
  blake3_compress_in_place:= @blake3_compress_in_place_portable;
  blake3_compress_xof:= @blake3_compress_xof_portable;
  blake3_hash_many:= @blake3_hash_many_portable;
{$ENDIF}
end.

