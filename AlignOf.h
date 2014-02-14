//===--- AlignOf.h - Portable calculation of type alignment -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the AlignOf function that computes alignments for
// arbitrary types.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_ALIGNOF_H
#define LLVM_SUPPORT_ALIGNOF_H

namespace llvm {

template <typename T>
struct AlignmentCalcImpl {
  char x;
  T t;
private:
  AlignmentCalcImpl() {} // Never instantiate.
};

// A templated class that contains an enum value representing the alignment of
// the template argument.  For example, AlignOf<int>::Alignment represents the
// alignment of type "int".  The alignment calculated is the minimum alignment,
// and not necessarily the "desired" alignment returned by GCC's __alignof__
// (for example).  Note that because the alignment is an enum value, it can be
// used as a compile-time constant (e.g., for template instantiation).
template <typename T>
struct AlignOf {
  enum { Alignment =
         static_cast<unsigned int>(sizeof(AlignmentCalcImpl<T>) - sizeof(T)) };
};

/// \brief Create a type with an aligned char buffer.
template<std::size_t Alignment, std::size_t Size>
struct AlignedCharArray;

#define LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(x) \
  template<std::size_t Size> \
  struct AlignedCharArray<x, Size> { \
    __attribute__((aligned(x))) char buffer[Size]; \
  };

LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(1)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(2)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(4)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(8)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(16)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(32)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(64)
LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT(128)

#undef LLVM_ALIGNEDCHARARRAY_TEMPLATE_ALIGNMENT


namespace detail {
template <typename T1,
          typename T2 = char, typename T3 = char, typename T4 = char,
          typename T5 = char, typename T6 = char, typename T7 = char>
class AlignerImpl {
  T1 t1; T2 t2; T3 t3; T4 t4; T5 t5; T6 t6; T7 t7;

  AlignerImpl(); // Never defined or instantiated.
};

template <typename T1,
          typename T2 = char, typename T3 = char, typename T4 = char,
          typename T5 = char, typename T6 = char, typename T7 = char>
union SizerImpl {
  char arr1[sizeof(T1)], arr2[sizeof(T2)], arr3[sizeof(T3)], arr4[sizeof(T4)],
       arr5[sizeof(T5)], arr6[sizeof(T6)], arr7[sizeof(T7)];
};
} // end namespace detail

template <typename T1,
          typename T2 = char, typename T3 = char, typename T4 = char,
          typename T5 = char, typename T6 = char, typename T7 = char>
struct AlignedCharArrayUnion : llvm::AlignedCharArray<
    AlignOf<detail::AlignerImpl<T1, T2, T3, T4, T5, T6, T7> >::Alignment,
    sizeof(detail::SizerImpl<T1, T2, T3, T4, T5, T6, T7>)> {
};

} // end namespace llvm

#endif
