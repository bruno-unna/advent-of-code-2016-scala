package adventofcode2016

import adventofcode2016.Day12.Instruction.*
import adventofcode2016.Util
import zio.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day12 extends ZIOAppDefault:

  enum Register:
    case a, b, c, d

  private object Register:
    def from(c: Char): Register = Register.fromOrdinal(c - 'a')

  enum Instruction:
    case cpy(src: Register | Int, dst: Register)
    case inc(dst: Register)
    case dec(dst: Register)
    case jnz(reg: Register | Int, offset: Int)

  private type Program = Vector[Instruction]

  case class Computer(registers: Map[Register, Int], pc: Int, program: Program):
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

  @tailrec
  private def execute(computer: Computer): Computer =
    if computer.pc < 0 || computer.pc >= computer.program.length then computer
    else execute(computer.step)

  def run: ZIO[ZIOAppArgs & Scope, Throwable, ExitCode] =
    for
      listing <- Util.readStrings("/12.txt")

      registers = Map[Register, Int](Register.a -> 0, Register.b -> 0, Register.c -> 0, Register.d -> 0)
      pc = 0
      program <- loadProgram(listing.toVector)

      initialComputer = Computer(registers, pc, program)
      finalComputer = execute(initialComputer)

      fixedRegisters = Map[Register, Int](Register.a -> 0, Register.b -> 0, Register.c -> 1, Register.d -> 0)
      fixedInitialComputer = Computer(fixedRegisters, pc, program)
      fixedFinalComputer = execute(fixedInitialComputer)

      _ <- Console.printLine(s"Part 1, value at register 'a' is ${finalComputer.registers(Register.a)}").ignore
      _ <- Console.printLine(s"Part 2, value at register 'a' is ${fixedFinalComputer.registers(Register.a)}").ignore
    yield ExitCode.success
