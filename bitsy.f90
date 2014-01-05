!> Module for SHA-256 hashing in fortran.
!!
!! @author     Mikael Leetmaa
!! @data       05 Jan 2014
!!
module bitsy
  use iso_c_binding

  ! Never use implicit declarations.
  implicit none

  ! Keep private what we can.
  private

  ! Defines the public interface.
  public sha256
  public dirty_sha256

  ! Public for the sake of unit-testing.
  public sha256b
  public ms0
  public ms1
  public cs0
  public cs1
  public maj
  public ch
  public swap32
  public swap64
  public swap64a
  public consume_chunk

  contains

    !> SHA-256 interface function.
    !! @param str : (in) The message to digest.
    !! @return    : The SHA-256 digest as a string of length 64.
    function sha256(str)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      character(len=64) :: sha256
      character(len=*), intent(in) :: str
      ! -----------------------------------
      ! Call the work horse with proper bit swapping.
      sha256 = sha256b(str, 1)
    end function sha256

    !> Quick and dirty SHA-256 interface function (no bit-swapping).
    !! @param str : (in) The message to digest.
    !! @return    : The SHA-256 digest as a string of length 64.
    function dirty_sha256(str)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      character(len=64) :: dirty_sha256
      character(len=*), intent(in) :: str
      ! -----------------------------------
      ! Call the work horse - no bit swapping.
      dirty_sha256 = sha256b(str, 0)
    end function dirty_sha256

    !> Calculate the SHA-256 hash of the incomming string.
    !! @param str    : (in) The message to take digest.
    !! @param swap   : (in) Flag to indicate if swapping to big-endian
    !!                 input (swap=1) should be used. swap=1 is needed
    !!                 for the routine to pass the standard tests, but
    !!                 decreases speed with a factor 2.
    !! @return       : The SHA-256 digest as a string of length 64.
    function sha256b(str, swap)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      character(len=64) :: sha256b
      character(len=*), intent(in) :: str
      integer,          intent(in) :: swap
      ! -----------------------------------

      ! Helper variables.
      integer(kind=c_int64_t) :: length
      integer(kind=c_int32_t) :: temp1
      integer(kind=c_int32_t) :: temp2
      integer(kind=c_int32_t) :: i
      integer :: break
      integer :: pos0

      ! Parameters for the cruncher.
      integer(kind=c_int32_t) :: h0_ref(8)
      integer(kind=c_int32_t) :: k0_ref(64)

      ! Work areas.
      integer(kind=c_int32_t) :: h0(8)
      integer(kind=c_int32_t) :: k0(64)
      integer(kind=c_int32_t) :: a0(8)
      integer(kind=c_int32_t) :: w0(64)

      ! Set the initial data.
      data (h0_ref(i),i=1,8)/&
           & z'6a09e667', z'bb67ae85', z'3c6ef372', z'a54ff53a', z'510e527f', z'9b05688c', z'1f83d9ab', z'5be0cd19'/

      data (k0_ref(i), i=1,64)/&
           & z'428a2f98', z'71374491', z'b5c0fbcf', z'e9b5dba5', z'3956c25b', z'59f111f1', z'923f82a4', z'ab1c5ed5',&
           & z'd807aa98', z'12835b01', z'243185be', z'550c7dc3', z'72be5d74', z'80deb1fe', z'9bdc06a7', z'c19bf174',&
           & z'e49b69c1', z'efbe4786', z'0fc19dc6', z'240ca1cc', z'2de92c6f', z'4a7484aa', z'5cb0a9dc', z'76f988da',&
           & z'983e5152', z'a831c66d', z'b00327c8', z'bf597fc7', z'c6e00bf3', z'd5a79147', z'06ca6351', z'14292967',&
           & z'27b70a85', z'2e1b2138', z'4d2c6dfc', z'53380d13', z'650a7354', z'766a0abb', z'81c2c92e', z'92722c85',&
           & z'a2bfe8a1', z'a81a664b', z'c24b8b70', z'c76c51a3', z'd192e819', z'd6990624', z'f40e3585', z'106aa070',&
           & z'19a4c116', z'1e376c08', z'2748774c', z'34b0bcb5', z'391c0cb3', z'4ed8aa4a', z'5b9cca4f', z'682e6ff3',&
           & z'748f82ee', z'78a5636f', z'84c87814', z'8cc70208', z'90befffa', z'a4506ceb', z'bef9a3f7', z'c67178f2'/

      h0 = h0_ref
      k0 = k0_ref
      ! -----------------------------------
      ! Function body implementation.

      break  = 0
      pos0   = 1
      length = len(trim(str))

      do while (break .ne. 1)

         ! Get the next 16 32bit words to consume.
         call consume_chunk(str, length, w0(1:16), pos0, break, swap)

         ! Extend the first 16 words to fill the work schedule array.
         do i=17,64
            w0(i) = ms1(w0(i-2)) + w0(i-16) + ms0(w0(i-15)) + w0(i-7)
         end do

         ! Initialize the workin variables with the current version of the hash.
         a0 = h0

         ! Run the compression loop.
         do i=1,64

            temp1 = a0(8) + cs1(a0(5)) + ch(a0(5),a0(6),a0(7)) + k0(i) + w0(i)
            temp2 = cs0(a0(1)) + maj(a0(1),a0(2),a0(3))

            a0(8) = a0(7)
            a0(7) = a0(6)
            a0(6) = a0(5)
            a0(5) = a0(4) + temp1
            a0(4) = a0(3)
            a0(3) = a0(2)
            a0(2) = a0(1)
            a0(1) = temp1 + temp2

         end do

         ! Update the state.
         h0 = h0 + a0

      end do

      ! Write the result to the output variable.
      write(sha256b,'(8z8)') h0(1), h0(2), h0(3), h0(4), h0(5), h0(6), h0(7), h0(8)

    end function sha256b

    !> Swap the byte order on a 32bit integer.
    !! @param inp : (in) The integer to byte swap.
    !! @return    : The byte swapped integer.
    function swap32(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: swap32
      integer(kind=c_int32_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp, 24, 8, swap32,  0)
      call mvbits(inp, 16, 8, swap32,  8)
      call mvbits(inp,  8, 8, swap32, 16)
      call mvbits(inp,  0, 8, swap32, 24)
    end function swap32

    !> Swap the byte order on a 64 bit integer.
    !! @param inp : (in) The integer to byte swap.
    !! @return    : The byte swapped integer.
    function swap64(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int64_t) :: swap64
      integer(kind=c_int64_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp, 56, 8, swap64,  0)
      call mvbits(inp, 48, 8, swap64,  8)
      call mvbits(inp, 40, 8, swap64, 16)
      call mvbits(inp, 32, 8, swap64, 24)
      call mvbits(inp, 24, 8, swap64, 32)
      call mvbits(inp, 16, 8, swap64, 40)
      call mvbits(inp,  8, 8, swap64, 48)
      call mvbits(inp,  0, 8, swap64, 56)
    end function swap64

    !> Swap the byte order on a 64bit integer as if
    !! each half was a 32bit integer to swap.
    !! @param inp : (in) The integer to byte swap.
    !! @return    : The byte swapped integer.
    function swap64a(inp)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int64_t) :: swap64a
      integer(kind=c_int64_t), intent(in)  :: inp
      ! -----------------------------------
      call mvbits(inp,  0, 8, swap64a, 32)
      call mvbits(inp,  8, 8, swap64a, 40)
      call mvbits(inp, 16, 8, swap64a, 48)
      call mvbits(inp, 24, 8, swap64a, 56)
      call mvbits(inp, 32, 8, swap64a,  0)
      call mvbits(inp, 40, 8, swap64a,  8)
      call mvbits(inp, 48, 8, swap64a, 16)
      call mvbits(inp, 56, 8, swap64a, 24)
    end function swap64a

    !> The 'ch' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @param b : (in) The b input integer.
    !! @param c : (in) The c input integer.
    !! @return  : ch(a,b,c), see the code.
    function ch(a, b, c)
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ch
      integer(kind=c_int32_t), intent(in) :: a
      integer(kind=c_int32_t), intent(in) :: b
      integer(kind=c_int32_t), intent(in) :: c
      ! -----------------------------------
      ch = ieor(iand(a, b), (iand(not(a), c)))
    end function ch

    !> The 'maj' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @param b : (in) The b input integer.
    !! @param c : (in) The c input integer.
    !! @return  : maj(a,b,c), see the code.
    function maj(a, b, c)
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: maj
      integer(kind=c_int32_t), intent(in) :: a
      integer(kind=c_int32_t), intent(in) :: b
      integer(kind=c_int32_t), intent(in) :: c
      ! -----------------------------------
      maj = ieor(iand(a, b), ieor(iand(a, c), iand(b, c)))
    end function maj

    !> The '\Sigma_0' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @return  : cs0(a), see the code.
    function cs0(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: cs0
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      cs0 = ieor(ishftc(a, -2), ieor(ishftc(a, -13), ishftc(a, -22)))
    end function cs0

    !> The '\Sigma_1' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @return  : cs1(a), see the code.
    function cs1(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: cs1
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      cs1 = ieor(ishftc(a, -6), ieor(ishftc(a, -11), ishftc(a, -25)))
    end function cs1

    !> The '\sigma_0' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @return  : ms0(a), see the code.
    function ms0(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ms0
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      ms0 = ieor(ishftc(a, -7), ieor(ishftc(a, -18), ishft(a, -3)))
    end function ms0

    !> The '\sigma_1' function in SHA-2.
    !! @param a : (in) The a input integer.
    !! @return  : ms1(a), see the code.
    function ms1(a)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      integer(kind=c_int32_t) :: ms1
      integer(kind=c_int32_t), intent(in) :: a
      ! -----------------------------------
      ms1 = ieor(ishftc(a, -17), ieor(ishftc(a, -19), ishft(a, -10)))
    end function ms1

    !> Copy 16 32bit words of data from str(pos0) to inp(1:16). The
    !! data is padded a requiered by the SHA-256 algorithm.
    !! @param str    : (in) The message to take a chunk from.
    !! @param length : (in) The length of the message in 8bit words.
    !! @param inp    : (inout) The work area to copy the data to.
    !! @param pos0   : (inout) Variable to store the start of the next chunk.
    !! @param break  : (inout) Indicates the position in the work flow.
    !!                 break=0 on entry -> continue to consume a chunk, pad if needed.
    !!                 break=2 on entry -> continue to consume, padding was allready done.
    !!                 break=1 one exit -> the last chunk was consumed.
    !! @param swap   : (in) Flag to indicate if swapping to big-endian
    !!                 input (swap=1) should be used. swap=1 is needed
    !!                 for the routine to pass the standard tests, but
    !!                 decreases speed with a factor 2.
   subroutine consume_chunk(str, length, inp, pos0, break, swap)
      implicit none
      ! -----------------------------------
      ! Define the interface.
      character(len=*),        intent(in)    :: str
      integer(kind=c_int64_t), intent(in)    :: length
      integer(kind=c_int32_t), intent(inout) :: inp(*)
      integer,                 intent(inout) :: pos0
      integer,                 intent(inout) :: break
      integer,                 intent(in)    :: swap
      ! -----------------------------------
      ! Internal variables.
      character(len=4)        :: last_word
      integer(kind=c_int64_t) :: rest
      integer(kind=c_int32_t) :: to_pad
      integer(kind=c_int32_t) :: leftover
      integer(kind=c_int32_t) :: space_left
      integer(kind=c_int32_t) :: zero
      integer(kind=c_int8_t)  :: ipad0
      integer(kind=c_int8_t)  :: ipad1
      integer(kind=c_int8_t)  :: i
      data zero  / b'00000000000000000000000000000000'/
      data ipad0 / b'00000000' /
      data ipad1 / b'10000000' /

      ! Calculate the rest.
      rest = length - pos0 + 1

      ! If we are far from the end.
      if (rest .ge. 64) then

         ! Copy the data over.
         inp(1:16) = transfer(str(pos0:pos0+64-1), inp(1:16))

         ! Big-endian.
         if (swap .eq. 1) then
            do i=1,16
               inp(i) = swap32(inp(i))
            end do
         end if

         ! Increment the starting position for the next roundx.
         pos0 = pos0 + 64

      else
         ! Space left in the input chunk.
         space_left = 16

         ! number of leftover full 32bit words.
         leftover   = rest/4

         ! Copy any leftovers.
         if (leftover .gt. 0) then
            inp(1:leftover) = transfer(str(pos0:pos0+leftover*4-1), inp(1:16))

            ! Big-endian.
            if (swap .eq. 1) then
               do i=1,leftover
                  inp(i) = swap32(inp(i))
               end do
            end if

            ! Increment the starting position.
            pos0 = pos0 + leftover*4
            rest = length - pos0 + 1
            space_left = space_left - leftover

         end if

         if (space_left .gt. 0) then

            if (break .ne. 2) then
               ! Add any remaining incomplete 32bit word.
               if (rest .gt. 0) then
                  last_word(1:rest) = str(pos0:pos0+rest-1)
                  ! Increment the pos0.
                  pos0 = pos0 + rest
               end if

               ! Add the '10000000' padding.
               last_word(rest+1:rest+1) = transfer(ipad1, last_word(1:1))

               ! Add zeros for a full 32bit word.
               to_pad = 4 - rest - 1
               do i=1,to_pad
                  last_word(rest+1+i:rest+1+i) = transfer(ipad0, last_word(1:1))
               end do

               ! Copy the last full (padded) word over.
               inp(17-space_left) = transfer(last_word(1:4), inp(1))

               if (swap .eq. 1) then
                  inp(17-space_left) = swap32(inp(17-space_left))
               end if

               ! Decrement the space left.
               space_left = space_left - 1

               ! Set the flag to indicate that we have padded.
               break = 2

            end if

            ! If not enough space to finnish, add zeros.
            if (space_left .eq. 1) then
               inp(16) = zero
               space_left = 0
            end if

            rest = 0

         end if

         ! Continue with the last part if there is enough space left.
         if ((rest .eq. 0) .and. (space_left .ge. 2)) then

            ! Add zeros until 64 bits left.
            do while (space_left .gt. 2)
               inp(17-space_left) = zero
               space_left = space_left - 1
            end do

            ! Add the two last 32bit words.
            inp(15:16) = transfer(swap64a(length*8), inp(15:16))

            ! Set break flag indicating we are done with the whole message.
            break = 1

         end if

      end if

    end subroutine consume_chunk

end module bitsy

