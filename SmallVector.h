//===- llvm/ADT/SmallVector.h - 'Normally small' vectors --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the SmallVector class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_SMALLVECTOR_H
#define LLVM_ADT_SMALLVECTOR_H

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <memory>

#include "AlignOf.h"

namespace llvm {

/// NextPowerOf2 - Returns the next power of two (in 64-bits)
/// that is strictly greater than A.  Returns zero on overflow.
inline uint64_t NextPowerOf2(uint64_t A) {
  A |= (A >> 1);
  A |= (A >> 2);
  A |= (A >> 4);
  A |= (A >> 8);
  A |= (A >> 16);
  A |= (A >> 32);
  return A + 1;
}

/// isPodLike - This is a type trait that is used to determine whether a given
/// type can be copied around with memcpy instead of running ctors etc.
template <typename T>
struct isPodLike {
  // If the compiler supports the is_trivially_copyable trait use it, as it
  // matches the definition of isPodLike closely.
  static const bool value = __is_trivially_copyable(T);
};

// std::pair's are pod-like if their elements are.
template<typename T, typename U>
struct isPodLike<std::pair<T, U> > {
  static const bool value = isPodLike<T>::value && isPodLike<U>::value;
};
  

/// SmallVectorBase - This is all the non-templated stuff common to all
/// SmallVectors.
class SmallVectorBase {
protected:
  void *BeginX, *EndX, *CapacityX;

protected:
  SmallVectorBase(void *FirstEl, size_t Size)
    : BeginX(FirstEl), EndX(FirstEl), CapacityX((char*)FirstEl+Size) {}

  /// grow_pod - This is an implementation of the grow() method which only works
  /// on POD-like data types and is out of line to reduce code duplication.
  void grow_pod(void *FirstEl, size_t MinSizeInBytes, size_t TSize);

public:
  /// size_in_bytes - This returns size()*sizeof(T).
  size_t size_in_bytes() const {
    return size_t((char*)EndX - (char*)BeginX);
  }

  /// capacity_in_bytes - This returns capacity()*sizeof(T).
  size_t capacity_in_bytes() const {
    return size_t((char*)CapacityX - (char*)BeginX);
  }

  bool empty() const { return BeginX == EndX; }
};

template <typename T, unsigned N> struct SmallVectorStorage;


/// SmallVectorTemplateCommon - This is the part of SmallVectorTemplateBase
/// which does not depend on whether the type T is a POD. The extra dummy
/// template argument is used by ArrayRef to avoid unnecessarily requiring T
/// to be complete.
template <typename T, typename = void>
class SmallVectorTemplateCommon : public SmallVectorBase {
private:
  template <typename, unsigned> friend struct SmallVectorStorage;

  // Allocate raw space for N elements of type T.  If T has a ctor or dtor, we
  // don't want it to be automatically run, so we need to represent the space as
  // something else.  Use an array of char of sufficient alignment.
  typedef llvm::AlignedCharArrayUnion<T> U;
  U FirstEl;
  // Space after 'FirstEl' is clobbered, do not add any instance vars after it.

protected:
  SmallVectorTemplateCommon(size_t Size) : SmallVectorBase(&FirstEl, Size) {}

  void grow_pod(size_t MinSizeInBytes, size_t TSize) {
    SmallVectorBase::grow_pod(&FirstEl, MinSizeInBytes, TSize);
  }

  /// isSmall - Return true if this is a smallvector which has not had dynamic
  /// memory allocated for it.
  bool isSmall() const {
    return BeginX == static_cast<const void*>(&FirstEl);
  }

  void setEnd(T *P) { this->EndX = P; }
public:
  typedef size_t size_type;
  typedef T *iterator;
  typedef const T *const_iterator;

  typedef T &reference;
  typedef const T &const_reference;
  typedef T *pointer;
  typedef const T *const_pointer;

  // forward iterator creation methods.
  iterator begin() { return (iterator)this->BeginX; }
  const_iterator begin() const { return (const_iterator)this->BeginX; }
  iterator end() { return (iterator)this->EndX; }
  const_iterator end() const { return (const_iterator)this->EndX; }
public:

  size_type size() const { return end()-begin(); }

  /// capacity - Return the total number of elements in the currently allocated
  /// buffer.
  size_t capacity() const { return this->CapacityX - this->BeginX; }

  reference operator[](unsigned idx) {
    assert(begin() + idx < end());
    return begin()[idx];
  }
  const_reference operator[](unsigned idx) const {
    assert(begin() + idx < end());
    return begin()[idx];
  }
};

/// SmallVectorTemplateBase<isPodLike = false> - This is where we put method
/// implementations that are designed to work with non-POD-like T's.
template <typename T, bool isPodLike>
class SmallVectorTemplateBase : public SmallVectorTemplateCommon<T> {
protected:
  SmallVectorTemplateBase(size_t Size) : SmallVectorTemplateCommon<T>(Size) {}

  static void destroy_range(T *S, T *E) {
    while (S != E) {
      --E;
      E->~T();
    }
  }

  /// uninitialized_copy - Copy the range [I, E) onto the uninitialized
  /// memory starting with "Dest", constructing elements as needed.
  template<typename It1, typename It2>
  static void uninitialized_copy(It1 I, It1 E, It2 Dest) {
    std::uninitialized_copy(I, E, Dest);
  }

  /// grow - Grow the allocated memory (without initializing new
  /// elements), doubling the size of the allocated memory.
  /// Guarantees space for at least one more element, or MinSize more
  /// elements if specified.
  void grow(size_t MinSize = 0);
  
public:
  void push_back(const T &Elt) {
    if (this->EndX < this->CapacityX) {
    Retry:
      ::new ((void*) this->end()) T(Elt);
      this->setEnd(this->end()+1);
      return;
    }
    this->grow();
    goto Retry;
  }

  void pop_back() {
    this->setEnd(this->end()-1);
    this->end()->~T();
  }
};

// Define this out-of-line to dissuade the C++ compiler from inlining it.
template <typename T, bool isPodLike>
void SmallVectorTemplateBase<T, isPodLike>::grow(size_t MinSize) {
  size_t CurCapacity = this->capacity();
  size_t CurSize = this->size();
  // Always grow, even from zero.  
  size_t NewCapacity = size_t(NextPowerOf2(CurCapacity+2));
  if (NewCapacity < MinSize)
    NewCapacity = MinSize;
  T *NewElts = static_cast<T*>(malloc(NewCapacity*sizeof(T)));

  // Move the elements over.
  this->uninitialized_copy(this->begin(), this->end(), NewElts);

  // Destroy the original elements.
  destroy_range(this->begin(), this->end());

  // If this wasn't grown from the inline copy, deallocate the old space.
  if (!this->isSmall())
    free(this->begin());

  this->setEnd(NewElts+CurSize);
  this->BeginX = NewElts;
  this->CapacityX = this->begin()+NewCapacity;
}


/// SmallVectorTemplateBase<isPodLike = true> - This is where we put method
/// implementations that are designed to work with POD-like T's.
template <typename T>
class SmallVectorTemplateBase<T, true> : public SmallVectorTemplateCommon<T> {
protected:
  SmallVectorTemplateBase(size_t Size) : SmallVectorTemplateCommon<T>(Size) {}

  // No need to do a destroy loop for POD's.
  static void destroy_range(T *, T *) {}

  /// uninitialized_copy - Copy the range [I, E) onto the uninitialized memory
  /// starting with "Dest", constructing elements into it as needed.
  template<typename It1, typename It2>
  static void uninitialized_copy(It1 I, It1 E, It2 Dest) {
    // Arbitrary iterator types; just use the basic implementation.
    std::uninitialized_copy(I, E, Dest);
  }

  /// uninitialized_copy - Copy the range [I, E) onto the uninitialized memory
  /// starting with "Dest", constructing elements into it as needed.
  template<typename T1, typename T2>
  static void uninitialized_copy(T1 *I, T1 *E, T2 *Dest) {
    // Use memcpy for PODs iterated by pointers (which includes SmallVector
    // iterators): std::uninitialized_copy optimizes to memmove, but we can
    // use memcpy here.
    memcpy(Dest, I, (E-I)*sizeof(T));
  }

  /// grow - double the size of the allocated memory, guaranteeing space for at
  /// least one more element or MinSize if specified.
  void grow(size_t MinSize = 0) {
    this->grow_pod(MinSize*sizeof(T), sizeof(T));
  }
public:
  void push_back(const T &Elt) {
    if (this->EndX < this->CapacityX) {
    Retry:
      memcpy(this->end(), &Elt, sizeof(T));
      this->setEnd(this->end()+1);
      return;
    }
    this->grow();
    goto Retry;
  }
  
  void pop_back() {
    this->setEnd(this->end()-1);
  }
};


/// SmallVectorImpl - This class consists of common code factored out of the
/// SmallVector class to reduce code duplication based on the SmallVector 'N'
/// template parameter.
template <typename T>
class SmallVectorImpl : public SmallVectorTemplateBase<T, isPodLike<T>::value> {
  SmallVectorImpl(const SmallVectorImpl&);
  SmallVectorImpl &operator=(const SmallVectorImpl &RHS);

protected:
  // Default ctor - Initialize to empty.
  explicit SmallVectorImpl(unsigned N)
    : SmallVectorTemplateBase<T, isPodLike<T>::value>(N*sizeof(T)) {}

public:
  ~SmallVectorImpl() {
    // Destroy the constructed elements in the vector.
    this->destroy_range(this->begin(), this->end());

    // If this wasn't grown from the inline copy, deallocate the old space.
    if (!this->isSmall())
      free(this->begin());
  }
};


/// Storage for the SmallVector elements which aren't contained in
/// SmallVectorTemplateCommon. There are 'N-1' elements here. The remaining '1'
/// element is in the base class. This is specialized for the N=1 and N=0 cases
/// to avoid allocating unnecessary storage.
template <typename T, unsigned N>
struct SmallVectorStorage {
  typename SmallVectorTemplateCommon<T>::U InlineElts[N - 1];
};
template <typename T> struct SmallVectorStorage<T, 1> {};
template <typename T> struct SmallVectorStorage<T, 0> {};

/// SmallVector - This is a 'vector' (really, a variable-sized array), optimized
/// for the case when the array is small.  It contains some number of elements
/// in-place, which allows it to avoid heap allocation when the actual number of
/// elements is below that threshold.  This allows normal "small" cases to be
/// fast without losing generality for large inputs.
///
/// Note that this does not attempt to be exception safe.
///
template <typename T, unsigned N>
class SmallVector : public SmallVectorImpl<T> {
  /// Storage - Inline space for elements which aren't stored in the base class.
  SmallVectorStorage<T, N> Storage;
  SmallVector(const SmallVector &RHS);
  const SmallVector &operator=(const SmallVector &RHS);
public:
  SmallVector() : SmallVectorImpl<T>(N) {}
};

} // End llvm namespace

#endif
