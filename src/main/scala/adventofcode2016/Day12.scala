package adventofcode2016

import adventofcode2016.Day12.Instruction.*
import adventofcode2016.Day12.Register.a
import adventofcode2016.Util
import zio.*

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * A solution to Day 12 of Advent of Code 2016.
 *
 * This program simulates a simple computer with four registers and a set of instructions.
 * It reads a program from a file, executes it, and prints the final value of register 'a' for two different initial states.
 */
object Day12 extends ZIOAppDefault:

  /**
   * Represents the four registers of the computer.
   */
  enum Register:
    case a, b, c, d

  object Register:
    /**
     * Converts a character to its corresponding Register.
     *
     * @param c The character representing the register ('a' through 'd').
     * @return The corresponding Register enum case.
     */
    def from(c: Char): Register = Register.fromOrdinal(c - 'a')

  /**
   * Represents the instructions supported by the computer.
   */
  enum Instruction:
    /** Copies a value (from a register or an integer literal) to a destination register. */
    case cpy(src: Register | Int, dst: Register)
    /** Increments a register by one. */
    case inc(dst: Register)
    /** Decrements a register by one. */
    case dec(dst: Register)
    /** Jumps to a new instruction offset if the value (from a register or an integer literal) is not zero. */
    case jnz(reg: Register | Int, offset: Int)

  private type Program = Vector[Instruction]

  /**
   * Represents the state of the computer.
   *
   * @param registers A map holding the current values of the registers.
   * @param pc        The program counter, indicating the index of the next instruction to execute.
   * @param program   The list of instructions to be executed.
   */
  case class Computer(registers: Map[Register, Int], pc: Int, program: Program):
    /**
     * Executes the instruction at the current program counter and returns the new computer state.
     *
     * @return A new `Computer` instance representing the state after one step.
     */
    def step: Computer = program(pc) match
      case cpy(src: Register, dst: Register) => Computer(registers + (dst -> registers(src)), pc + 1, program)
      case cpy(src: Int, dst: Register) => Computer(registers + (dst -> src), pc + 1, program)
      case inc(dst: Register) => Computer(registers + (dst -> (registers(dst) + 1)), pc + 1, program)
      case dec(dst: Register) => Computer(registers + (dst -> (registers(dst) - 1)), pc + 1, program)
      case jnz(src: Register, offset: Int) => Computer(registers, if registers(src) == 0 then pc + 1 else pc + offset, program)
      case jnz(src: Int, offset: Int) => Computer(registers, if src == 0 then pc + 1 else pc + offset, program)

  private val CpyRE: Regex = """^cpy\s+(\d+|[a-d])\s+([a-d])$""".r
  private val IncRE: Regex = """^inc\s+([a-d])$""".r
  private val DecRE: Regex = """^dec\s+([a-d])$""".r
  private val JnzRE: Regex = """^jnz\s+(\d+|[a-d])\s+(-?\d+)$""".r

  /**
   * Parses a sequence of strings into a program (a vector of instructions).
   *
   * @param strings The list of instruction strings.
   * @return A ZIO task that succeeds with the parsed program or fails with an exception.
   */
  private def loadProgram(strings: Vector[String]): Task[Program] =
    ZIO.collectAll:
      strings.map:
        case CpyRE(src, dst) =>
          src.head match
            case 'a' | 'b' | 'c' | 'd' => ZIO.succeed(cpy(Register.from(src.head), Register.from(dst.head)))
            case _ => ZIO.attempt(cpy(src.toInt, Register.from(dst.head)))
        case IncRE(reg) => ZIO.succeed(inc(Register.from(reg.head)))
        case DecRE(reg) => ZIO.succeed(dec(Register.from(reg.head)))
        case JnzRE(src, offset) =>
          src.head match
            case 'a' | 'b' | 'c' | 'd' => ZIO.succeed(jnz(Register.from(src.head), offset.toInt))
            case _ => ZIO.attempt(jnz(src.toInt, offset.toInt))
        case other@_ => ZIO.fail(new Exception(s"Parsing error: ${other}"))

  /**
   * Recursively executes the program until the program counter goes out of bounds.
   *
   * @param computer The initial computer state.
   * @return The final computer state after execution.
   */
  @tailrec
  private def execute(computer: Computer): Computer =
    if computer.pc < 0 || computer.pc >= computer.program.length then computer
    else execute(computer.step)

  /**
   * The main method that runs the Advent of Code Day 12 solution.
   * It loads the program, executes it for two different initial conditions, and prints the results.
   */
  def run: ZIO[ZIOAppArgs & Scope, Throwable, ExitCode] =
    for
      listing <- Util.readStrings("/12.txt")
      program <- loadProgram(listing.toVector)

      registersA = for
        registers <- List(
          Map[Register, Int](Register.a -> 0, Register.b -> 0, Register.c -> 0, Register.d -> 0),
          Map[Register, Int](Register.a -> 0, Register.b -> 0, Register.c -> 1, Register.d -> 0)
        )
        pc = 0
        computer = Computer(registers, pc, program)
        newComputer = execute(computer)
      yield newComputer.registers(a)

      _ <- Console.printLine(s"Register a: ${registersA}")
    yield ExitCode.success
