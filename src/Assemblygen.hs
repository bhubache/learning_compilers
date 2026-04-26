module Assemblygen where

import Parse qualified

newtype AsmProgram = AsmProgram AsmFunction
  deriving (Show)

data AsmFunction = AsmFunction Identifier InstructionList
  deriving (Show)

newtype Identifier = Identifier String
  deriving (Show)

newtype InstructionList = InstructionList [Instruction]
  deriving (Show)

data Instruction = Mov Operand Operand | Ret
  deriving (Show)

data Operand = Immediate Integer | Register String

instance Show Operand where
  show (Immediate imm) = "$" ++ show imm
  show (Register reg) = "%" ++ reg

parseCAst :: Parse.Program -> AsmProgram
parseCAst (Parse.Program f) = AsmProgram $ genFunction f

genFunction :: Parse.Function -> AsmFunction
genFunction (Parse.Function rt (Parse.Identifier identifier) params body) = AsmFunction (Identifier identifier) (InstructionList $ genInstructionList body)

genInstructionList :: Parse.Body -> [Instruction]
genInstructionList (Parse.ReturnStatement (Parse.Constant imm)) = [Mov (Immediate imm) (Register "eax"), Ret]
