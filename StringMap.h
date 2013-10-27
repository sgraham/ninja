//===--- StringMap.h - String Hash table map interface ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the StringMap class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STRINGMAP_H
#define LLVM_ADT_STRINGMAP_H

#include "Allocator.h"
#include <cstring>

struct StringPiece {
  StringPiece(const char* str) : str_(str), len_(strlen(str)) {}
  StringPiece(const char* str, unsigned len) : str_(str), len_(len) {}
  bool operator==(const StringPiece& other) const {
    return len_ == other.len_ && memcmp(str_, other.str_, len_) == 0;
  }
  const char* str_;
  unsigned len_;
  const char* begin() { return str_; }
  const char* end() { return str_ + len_; }
};

namespace llvm {
template<typename ValueT> class StringMapConstIterator;
template<typename ValueT> class StringMapIterator;
template <typename ValueTy> class StringMapEntry;

// Shared base class of StringMapEntry instances.
class StringMapEntryBase {
  unsigned StrLen;
public:
  explicit StringMapEntryBase(unsigned Len) : StrLen(Len) {}
  unsigned getKeyLength() const { return StrLen; }
};

// This is the base class of StringMap that is shared among all of its
// instantiations.
class StringMapImpl {
protected:
  // Array of NumBuckets pointers to entries, null pointers are holes.
  // TheTable[NumBuckets] contains a sentinel value for easy iteration. Followed
  // by an array of the actual hash values as unsigned integers.
  StringMapEntryBase **TheTable;
  unsigned NumBuckets;
  unsigned NumItems;
  unsigned NumTombstones;
  unsigned ItemSize;
protected:
  explicit StringMapImpl(unsigned itemSize) : ItemSize(itemSize) {
    // Initialize the map with zero buckets to allocation.
    TheTable = 0;
    NumBuckets = 0;
    NumItems = 0;
    NumTombstones = 0;
  }
  StringMapImpl(unsigned InitSize, unsigned ItemSize);
  void RehashTable();

  // Look up the bucket that the specified string should end up in.  If it
  // already exists as a key in the map, the Item pointer for the specified
  // bucket will be non-null.  Otherwise, it will be null.  In either case, the
  // FullHashValue field of the bucket will be set to the hash value of the
  // string.
  unsigned LookupBucketFor(StringPiece Key);

  // Look up the bucket that contains the specified key. If it exists in the
  // map, return the bucket number of the key.  Otherwise return -1.
  // This does not modify the map.
  int FindKey(StringPiece Key) const;

  // Remove the specified StringMapEntry from the table, but do not delete it.
  // This aborts if the value isn't in the table.
  void RemoveKey(StringMapEntryBase *V);

  // Remove the StringMapEntry for the specified key from the table, returning
  // it.  If the key is not in the table, this returns null.
  StringMapEntryBase *RemoveKey(StringPiece Key);
private:
  void init(unsigned Size);
public:
  static StringMapEntryBase *getTombstoneVal() {
    return (StringMapEntryBase*)-1;
  }

  unsigned getNumBuckets() const { return NumBuckets; }
  unsigned getNumItems() const { return NumItems; }

  bool empty() const { return NumItems == 0; }
  unsigned size() const { return NumItems; }
};

// This is used to represent one value that is inserted into a StringMap.  It
// contains the Value itself and the key: the string length and data.
template <typename ValueTy> class StringMapEntry : public StringMapEntryBase {
public:
  ValueTy second;

  StringMapEntry(unsigned strLen, const ValueTy &V)
    : StringMapEntryBase(strLen), second(V) {}

  StringPiece getKey() const {
    return StringPiece(getKeyData(), getKeyLength());
  }

  ValueTy &getValue() { return second; }
  void setValue(const ValueTy &V) { second = V; }

  // Return the start of the string data that is the key for this value.  The
  // string data is always stored immediately after the StringMapEntry object.
  const char *getKeyData() const {return reinterpret_cast<const char*>(this+1);}

  // Create a StringMapEntry for the specified key and default construct the
  // value.
  template<typename AllocatorTy, typename InitType>
  static StringMapEntry *Create(const char *KeyStart, const char *KeyEnd,
                                AllocatorTy &Allocator, InitType InitVal) {
    unsigned KeyLength = static_cast<unsigned>(KeyEnd - KeyStart);
    unsigned AllocSize =
        static_cast<unsigned>(sizeof(StringMapEntry)) + KeyLength + 1;
    unsigned Alignment = AlignOf<StringMapEntry>::Alignment;

    StringMapEntry *NewItem =
        static_cast<StringMapEntry *>(Allocator.Allocate(AllocSize, Alignment));

    // Default construct the value.
    new (NewItem) StringMapEntry(KeyLength, InitVal);

    // Copy the string information.
    char *StrBuffer = const_cast<char*>(NewItem->getKeyData());
    memcpy(StrBuffer, KeyStart, KeyLength);
    StrBuffer[KeyLength] = 0;  // Null terminate for convenience of clients.
    return NewItem;
  }

  // Destroy this StringMapEntry, releasing memory back to the specified
  // allocator.
  template<typename AllocatorTy>
  void Destroy(AllocatorTy &Allocator) {
    // Free memory referenced by the item.
    this->~StringMapEntry();
    Allocator.Deallocate(this);
  }
};


// This is an unconventional map that is specialized for handling keys that are
// "strings", which are basically ranges of bytes. This does some funky memory
// allocation and hashing things to make it extremely efficient, storing the
// string data *after* the value in the map.
template<typename ValueTy, typename AllocatorTy = MallocAllocator>
class StringMap : public StringMapImpl {
  StringMap(const StringMap &RHS);
  void operator=(const StringMap &RHS);
  AllocatorTy Allocator;
public:
  typedef StringMapEntry<ValueTy> MapEntryTy;

  StringMap() : StringMapImpl(static_cast<unsigned>(sizeof(MapEntryTy))) {}
  ~StringMap() { clear(); free(TheTable); }

  typedef StringMapConstIterator<ValueTy> const_iterator;
  typedef StringMapIterator<ValueTy> iterator;

  iterator begin() { return iterator(TheTable, NumBuckets == 0); }
  iterator end() { return iterator(TheTable + NumBuckets, true); }
  const_iterator begin() const {
    return const_iterator(TheTable, NumBuckets == 0);
  }
  const_iterator end() const {
    return const_iterator(TheTable + NumBuckets, true);
  }

  AllocatorTy &getAllocator() { return Allocator; }

  iterator find(StringPiece Key) {
    int Bucket = FindKey(Key);
    if (Bucket == -1) return end();
    return iterator(TheTable+Bucket, true);
  }

  const_iterator find(StringPiece Key) const {
    int Bucket = FindKey(Key);
    if (Bucket == -1) return end();
    return const_iterator(TheTable+Bucket, true);
  }

  ValueTy &operator[](StringPiece Key) {
    return GetOrCreateValue(Key).getValue();
  }

  size_t count(StringPiece Key) const {
    return find(Key) == end() ? 0 : 1;
  }

  // Insert the specified key/value pair into the map.  If the key already
  // exists in the map, return false and ignore the request, otherwise insert
  // it and return true.
  bool insert(MapEntryTy *KeyValue) {
    unsigned BucketNo = LookupBucketFor(KeyValue->getKey());
    StringMapEntryBase *&Bucket = TheTable[BucketNo];
    if (Bucket && Bucket != getTombstoneVal())
      return false;  // Already exists in map.

    if (Bucket == getTombstoneVal())
      --NumTombstones;
    Bucket = KeyValue;
    ++NumItems;
    assert(NumItems + NumTombstones <= NumBuckets);

    RehashTable();
    return true;
  }

  // Empties out the StringMap
  void clear() {
    if (empty()) return;

    // Zap all values, resetting the keys back to non-present (not tombstone),
    // which is safe because we're removing all elements.
    for (unsigned I = 0, E = NumBuckets; I != E; ++I) {
      StringMapEntryBase *&Bucket = TheTable[I];
      if (Bucket && Bucket != getTombstoneVal()) {
        static_cast<MapEntryTy*>(Bucket)->Destroy(Allocator);
      }
      Bucket = 0;
    }

    NumItems = 0;
    NumTombstones = 0;
  }

  // Look up the specified key in the table.  If a value exists, return it.
  // Otherwise, default construct a value, insert it, and return.
  template <typename InitTy>
  MapEntryTy &GetOrCreateValue(StringPiece Key, InitTy Val) {
    unsigned BucketNo = LookupBucketFor(Key);
    StringMapEntryBase *&Bucket = TheTable[BucketNo];
    if (Bucket && Bucket != getTombstoneVal())
      return *static_cast<MapEntryTy*>(Bucket);

    // Okay, the item doesn't already exist, and 'Bucket' is the bucket to fill
    // in.  Allocate a new item with space for the string at the end and a null
    // terminator.

    MapEntryTy *NewItem =
        MapEntryTy::Create(Key.begin(), Key.end(), Allocator, Val);

    if (Bucket == getTombstoneVal())
      --NumTombstones;
    ++NumItems;
    assert(NumItems + NumTombstones <= NumBuckets);

    // Fill in the bucket for the hash table.  The FullHashValue was already
    // filled in by LookupBucketFor.
    Bucket = NewItem;

    RehashTable();
    return *NewItem;
  }

  MapEntryTy &GetOrCreateValue(StringPiece Key) {
    return GetOrCreateValue(Key, ValueTy());
  }

  // Remove the specified key/value pair from the map, but do not erase it.
  // This aborts if the key is not in the map.
  void remove(MapEntryTy *KeyValue) {
    RemoveKey(KeyValue);
  }

  void erase(iterator I) {
    MapEntryTy &V = *I;
    remove(&V);
    V.Destroy(Allocator);
  }

  bool erase(StringPiece Key) {
    iterator I = find(Key);
    if (I == end()) return false;
    erase(I);
    return true;
  }
};


template<typename ValueTy>
class StringMapConstIterator {
protected:
  StringMapEntryBase **Ptr;
public:
  typedef StringMapEntry<ValueTy> value_type;

  explicit StringMapConstIterator(StringMapEntryBase **Bucket,
                                  bool NoAdvance = false)
  : Ptr(Bucket) {
    if (!NoAdvance) AdvancePastEmptyBuckets();
  }

  const value_type &operator*() const {
    return *static_cast<StringMapEntry<ValueTy>*>(*Ptr);
  }
  const value_type *operator->() const {
    return static_cast<StringMapEntry<ValueTy>*>(*Ptr);
  }

  bool operator==(const StringMapConstIterator &RHS) const {
    return Ptr == RHS.Ptr;
  }
  bool operator!=(const StringMapConstIterator &RHS) const {
    return Ptr != RHS.Ptr;
  }

  inline StringMapConstIterator& operator++() {   // Preincrement
    ++Ptr;
    AdvancePastEmptyBuckets();
    return *this;
  }
  StringMapConstIterator operator++(int) {        // Postincrement
    StringMapConstIterator tmp = *this; ++*this; return tmp;
  }

private:
  void AdvancePastEmptyBuckets() {
    while (*Ptr == 0 || *Ptr == StringMapImpl::getTombstoneVal())
      ++Ptr;
  }
};

template<typename ValueTy>
class StringMapIterator : public StringMapConstIterator<ValueTy> {
public:
  explicit StringMapIterator(StringMapEntryBase **Bucket,
                             bool NoAdvance = false)
    : StringMapConstIterator<ValueTy>(Bucket, NoAdvance) {
  }
  StringMapEntry<ValueTy> &operator*() const {
    return *static_cast<StringMapEntry<ValueTy>*>(*this->Ptr);
  }
  StringMapEntry<ValueTy> *operator->() const {
    return static_cast<StringMapEntry<ValueTy>*>(*this->Ptr);
  }
};

}

#endif