! TEST SUITE FOR THE bitsy SHA-256 FORTRAN IMPLEMENTATION
! Author: Mikael Leetmaa
! Date:   05 Jan 2014
test_suite bitsy

integer(kind=4) :: ipad1, ipad2, ipad3, ipad4, ipad5, ipad6
integer(kind=4) :: shftc4_r2, shftc4_l12
integer(kind=4) :: shft5_r8, shft5_l11
integer(kind=4) :: abc_bin, a_bin, empty_str_bin, empty_bin
integer(kind=4) :: abc_bin_flip, a_bin_flip, empty_str_bin_flip
integer(kind=4) :: abc_bin_ref, abc_bin_swap

integer(kind=4) :: cabc_bin, abca_bin, bcab_bin, ca_one_zero
integer(kind=4) :: cabc_bin_flip, abca_bin_flip, bcab_bin_flip, ca_one_zero_flip
integer(kind=4) :: big_endian_464, little_endian_464

data ipad1 / b'00000000000000000000000000000011' /
data ipad2 / b'11111111111111111111111111111111' /
data ipad3 / b'10010000101001110011001110010011' /
data ipad4 / b'11001001101001110011001110010011' /
data ipad5 / b'10000001101001010011000110100001' /
data ipad6 / b'11000000000000000000000000000000' /

data shftc4_r2          / b'11110010011010011100110011100100' /
data shftc4_l12         / b'01110011001110010011110010011010' /
data shft5_r8           / b'00000000100000011010010100110001' /
data shft5_l11          / b'00101001100011010000100000000000' /
data abc_bin            / b'00000001011000110110001001100001' /
data a_bin              / b'00000000000000000000000101100001' /
data empty_str_bin      / b'00000000000000000000000000000001' /
data empty_bin          / b'00000000000000000000000000000000' /
data abc_bin_flip       / b'01100001011000100110001110000000' /
data empty_str_bin_flip / b'10000000000000000000000000000000' /
data a_bin_flip         / b'01100001100000000000000000000000' /

data abca_bin           / b'01100001011000110110001001100001' /
data bcab_bin           / b'01100010011000010110001101100010' /
data cabc_bin           / b'01100011011000100110000101100011' /
data ca_one_zero        / b'00000000000000010110000101100011' /
data big_endian_464     / b'11010000000000010000000000000000' /
data little_endian_464  / b'00000000000000000000000111010000' /

data abc_bin_ref        / b'00000001011000110110001001100001' /
data abc_bin_swap       / b'01100001011000100110001100000001' /

data abca_bin_flip      / b'01100001011000100110001101100001' /
data bcab_bin_flip      / b'01100010011000110110000101100010' /
data cabc_bin_flip      / b'01100011011000010110001001100011' /
data ca_one_zero_flip   / b'01100011011000011000000000000000' /

! Test the swap function.
test test_swap32
  Assert_Equal(swap32(abc_bin), abc_bin_swap)
  Assert_Equal(abc_bin, abc_bin_ref)
end test

! Make sure the intrinsic ishftc function does what we think.
test test_ishftc
  integer(kind=4) :: a
  a = ishftc(ipad4, -2)
  Assert_Equal(a, shftc4_r2)
  a = ishftc(ipad4, 12)
  Assert_Equal(a, shftc4_l12)
end test

! Make sure the intrinsic ishft function does what we think.
test test_ishft
  integer(kind=4) :: a
  a = ishft(ipad5, -8)
  Assert_Equal(a, shft5_r8)
  a = ishft(ipad5, 11)
  Assert_Equal(a, shft5_l11)
end test

! Test the message padding.
test pad_message1
  character(len=1000) :: str
  integer(kind=4) :: inp(16)
  integer(kind=8) :: length
  integer :: pos0, break
  integer :: swap = 1

  ! Set the message to "".
  str = ""
  pos0   = 1
  break  = 0
  length = 0
  call consume_chunk(str, length, inp, pos0, break, swap)

  ! Check the first word.
  Assert_Equal(inp(1), empty_str_bin_flip)

  ! Set the message to "abc".
  str = "abc"
  pos0   = 1
  break  = 0
  length = 3
  call consume_chunk(str, length, inp, pos0, break, swap)

  ! Check the first word.
  Assert_Equal(inp(1), abc_bin_flip)

  ! Set the message to "a".
  str = "a"
  pos0   = 1
  break  = 0
  length = 1
  call consume_chunk(str, length, inp, pos0, break, swap)

  ! Check the first word.
  Assert_Equal(inp(1), a_bin_flip)

end test

! Test the message padding.
test pad_message2
  character(len=1024) :: str
  integer(kind=4) :: inp(16)
  integer(kind=8) :: length
  integer :: pos0, break
  integer :: swap = 1

  ! Set the message.
  str = "abcabcabcabcabcaabcabcabcabcabcaabcabcabcabcabcaabcabcabca"

  pos0   = 1
  break  = 0
  length = 58
  call consume_chunk(str, length, inp, pos0, break, swap)

  ! Check the whole message.
  Assert_Equal(inp(1),  abca_bin_flip)
  Assert_Equal(inp(2),  bcab_bin_flip)
  Assert_Equal(inp(3),  cabc_bin_flip)
  Assert_Equal(inp(4),  abca_bin_flip)
  Assert_Equal(inp(5),  abca_bin_flip)
  Assert_Equal(inp(6),  bcab_bin_flip)
  Assert_Equal(inp(7),  cabc_bin_flip)
  Assert_Equal(inp(8),  abca_bin_flip)
  Assert_Equal(inp(9),  abca_bin_flip)
  Assert_Equal(inp(10), bcab_bin_flip)
  Assert_Equal(inp(11), cabc_bin_flip)
  Assert_Equal(inp(12), abca_bin_flip)
  Assert_Equal(inp(13), abca_bin_flip)
  Assert_Equal(inp(14), bcab_bin_flip)
  Assert_Equal(inp(15), ca_one_zero_flip)
  Assert_Equal(inp(16), empty_bin)

  call consume_chunk(str, length, inp, pos0, break, swap)

  Assert_Equal(inp(1),  empty_bin)
  Assert_Equal(inp(2),  empty_bin)
  Assert_Equal(inp(3),  empty_bin)
  Assert_Equal(inp(4),  empty_bin)
  Assert_Equal(inp(5),  empty_bin)
  Assert_Equal(inp(6),  empty_bin)
  Assert_Equal(inp(7),  empty_bin)
  Assert_Equal(inp(8),  empty_bin)
  Assert_Equal(inp(9),  empty_bin)
  Assert_Equal(inp(10), empty_bin)
  Assert_Equal(inp(11), empty_bin)
  Assert_Equal(inp(12), empty_bin)
  Assert_Equal(inp(13), empty_bin)
  Assert_Equal(inp(14), empty_bin)
  Assert_Equal(inp(15), empty_bin)
  Assert_Equal(inp(16), little_endian_464)

end test

! Test the ch function.
test test_ch
  integer(kind=4) :: e, f, g
  integer(kind=4) :: aa, bb, cc
  e = ipad1
  f = ipad2
  g = ipad3
  aa = iand(not(e),g)
  bb = iand(e,f)
  Assert_Equal(ieor(aa,bb), maj(e,f,g))
end test

! Test the maj function.
test test_maj
  integer(kind=4) :: a, b, c
  integer(kind=4) :: aa, bb, cc

  a = ipad1
  b = ipad2
  c = ipad3
  aa = iand(a,b)
  bb = iand(a,c)
  cc = iand(b,c)
  Assert_Equal(ieor(aa, ieor(bb, cc)), maj(a,b,c))

  a = ipad2
  b = ipad3
  c = ipad4
  aa = iand(a,b)
  bb = iand(a,c)
  cc = iand(b,c)
  Assert_Equal(ieor(aa, ieor(bb, cc)), maj(a,b,c))

  a = ipad3
  b = ipad4
  c = ipad5
  aa = iand(a,b)
  bb = iand(a,c)
  cc = iand(b,c)
  Assert_Equal(ieor(aa, ieor(bb, cc)), maj(a,b,c))

  a = ipad4
  b = ipad5
  c = ipad6
  aa = iand(a,b)
  bb = iand(a,c)
  cc = iand(b,c)
  Assert_Equal(ieor(aa, ieor(bb, cc)), maj(a,b,c))

end test

! Test the major sigma-0 function.
test test_cs0
  integer(kind=4) :: a, b, c
  a   = ishftc(ipad1, -2)
  b   = ishftc(ipad1, -13)
  c   = ishftc(ipad1, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad1))

  a   = ishftc(ipad2, -2)
  b   = ishftc(ipad2, -13)
  c   = ishftc(ipad2, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad2))

  a   = ishftc(ipad3, -2)
  b   = ishftc(ipad3, -13)
  c   = ishftc(ipad3, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad3))

  a   = ishftc(ipad4, -2)
  b   = ishftc(ipad4, -13)
  c   = ishftc(ipad4, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad4))

  a   = ishftc(ipad5, -2)
  b   = ishftc(ipad5, -13)
  c   = ishftc(ipad5, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad5))

  a   = ishftc(ipad6, -2)
  b   = ishftc(ipad6, -13)
  c   = ishftc(ipad6, -22)
  Assert_Equal(ieor(a, ieor(b, c)), cs0(ipad6))
end test

! Test the major sigma-1 function.
test test_cs1
  integer(kind=4) :: a, b, c
  a   = ishftc(ipad1, -6)
  b   = ishftc(ipad1, -11)
  c   = ishftc(ipad1, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad1))

  a   = ishftc(ipad2, -6)
  b   = ishftc(ipad2, -11)
  c   = ishftc(ipad2, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad2))

  a   = ishftc(ipad3, -6)
  b   = ishftc(ipad3, -11)
  c   = ishftc(ipad3, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad3))

  a   = ishftc(ipad4, -6)
  b   = ishftc(ipad4, -11)
  c   = ishftc(ipad4, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad4))

  a   = ishftc(ipad5, -6)
  b   = ishftc(ipad5, -11)
  c   = ishftc(ipad5, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad5))

  a   = ishftc(ipad6, -6)
  b   = ishftc(ipad6, -11)
  c   = ishftc(ipad6, -25)
  Assert_Equal(ieor(a, ieor(b, c)), cs1(ipad6))

end test

! Test the minor sigma-0 function.
test test_ms0
  integer(kind=4) :: a, b, c

  a   = ishftc(ipad1, -7)
  b   = ishftc(ipad1, -18)
  c   =  ishft(ipad1, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad1))

  a   = ishftc(ipad2, -7)
  b   = ishftc(ipad2, -18)
  c   =  ishft(ipad2, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad2))

  a   = ishftc(ipad3, -7)
  b   = ishftc(ipad3, -18)
  c   =  ishft(ipad3, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad3))

  a   = ishftc(ipad4, -7)
  b   = ishftc(ipad4, -18)
  c   =  ishft(ipad4, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad4))

  a   = ishftc(ipad5, -7)
  b   = ishftc(ipad5, -18)
  c   =  ishft(ipad5, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad5))

  a   = ishftc(ipad6, -7)
  b   = ishftc(ipad6, -18)
  c   =  ishft(ipad6, -3)
  Assert_Equal(ieor(a, ieor(b, c)), ms0(ipad6))

end test

! Test the minor sigma-1 function.
test test_ms1
  integer(kind=4) :: a, b, c

  a   = ishftc(ipad1, -17)
  b   = ishftc(ipad1, -19)
  c   =  ishft(ipad1, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad1))

  a   = ishftc(ipad2, -17)
  b   = ishftc(ipad2, -19)
  c   =  ishft(ipad2, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad2))

  a   = ishftc(ipad3, -17)
  b   = ishftc(ipad3, -19)
  c   =  ishft(ipad3, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad3))

  a   = ishftc(ipad4, -17)
  b   = ishftc(ipad4, -19)
  c   =  ishft(ipad4, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad4))

  a   = ishftc(ipad5, -17)
  b   = ishftc(ipad5, -19)
  c   =  ishft(ipad5, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad5))

  a   = ishftc(ipad6, -17)
  b   = ishftc(ipad6, -19)
  c   =  ishft(ipad6, -10)
  Assert_Equal(ieor(a, ieor(b, c)), ms1(ipad6))

end test

! Test the sha256 function with a set of reference strings.
test test_sha256_1
  character(len=1000000) :: str
  str = ""
  Assert_Equal(sha256(str), "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855")
end test

test test_sha256_2
  character(len=1000000) :: str
  str = "abc"
  Assert_Equal(sha256(str), "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")
end test

test test_sha256_3
  character(len=1000000) :: str
  str = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  Assert_Equal(sha256(str), "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1")
end test

test test_sha256_4
  character(len=1000000) :: str
  str = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
  Assert_Equal(sha256(str), "CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1")
end test

test test_sha256_5
  character(len=1000000) :: str
  character(len=64)      :: ref
  integer :: i
  do i=1,1000000
     str(i:i) = "a"
  end do
  Assert_Equal(sha256(str), "CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0")
  ! Check the quick and dirty implementation as well.
  ref = "69E3FACD5F08321F78117BD53476E5321845433356F106E7013E68EC367F3017"
  Assert_Equal(dirty_sha256(str), ref)
end test

test test_sha256_6
  character(len=1000000) :: str
  str = "message digest"
  Assert_Equal(sha256(str), "F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650")
end test

test test_sha256_7
  character(len=1000000) :: str
  str = "secure hash algorithm"
  Assert_Equal(sha256(str), "F30CEB2BB2829E79E4CA9753D35A8ECC00262D164CC077080295381CBD643F0D")
end test

test test_sha256_8
  character(len=1000000) :: str
  str = "SHA256 is considered to be safe"
  Assert_Equal(sha256(str), "6819D915C73F4D1E77E4E1B52D1FA0F9CF9BEAEAD3939F15874BD988E2A23630")
end test

test test_sha256_9
  character(len=1000000) :: str
  str = "For this sample, this 63-byte string will be used as input data"
  Assert_Equal(sha256(str), "F08A78CBBAEE082B052AE0708F32FA1E50C5C421AA772BA5DBB406A2EA6BE342")
end test

test test_sha256_10
  character(len=1000000) :: str
  str = "This is exactly 64 bytes long, not counting the terminating byte"
  Assert_Equal(sha256(str), "AB64EFF7E88E2E46165E29F2BCE41826BD4C7B3552F6B382A9E7D3AF47C245F8")
end test

test test_sha256_11
  character(16777216*64) :: str
  integer :: i
  do i=1,16777216
     str(1+(i-1)*64:i*64) = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
  end do
  Assert_Equal(sha256(str), "50E72A0E26442FE2552DC3938AC58658228C0CBFB1D2CA872AE435266FCD055E")
end test

end test_suite
