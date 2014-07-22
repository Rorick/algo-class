
val s = "1 0 1 1"

val v = Integer parseInt(s replace(" ", ""), 2)

bitFlipPositions(1) map toMask map (flipBits(v, _)) map toBitString

def bitFlipPositions(n: Int): List[List[Int]] = {
  if (n == 0)
    List(List.empty)
  else for {
    position <- 0 until 4
    restBitPositions <- bitFlipPositions(n - 1)
    if restBitPositions.isEmpty || position < restBitPositions.head
  } yield position :: restBitPositions
}.toList

/**
 * Converts a number of bit positions starting from zero to mask with ones in specified bit positions.
 *
 * @param bitPositions list of bit positions to be flipped
 * @return binary mask with ones set in specified positions
 */
def toMask(bitPositions: List[Int]): Int = {
  bitPositions map (1 << _) reduce (_ | _)
}

bitFlipPositions(2) map toMask map (flipBits(v, _)) map toBitString

def fromBitString(s: String): Int = Integer.parseInt(s, 2)

def toBitString(i: Int): String = "%24s".format(Integer.toBinaryString(i)).replace(' ', '0')

/**
 * Flip bits in positions specified by mask
 *
 * @param v value whose bits are to be flipped
 * @param mask mask specifying bits to be flipped
 * @return value with flipped bits
 */
def flipBits(v: Int, mask: Int): Int = v ^ mask