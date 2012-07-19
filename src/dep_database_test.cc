// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifdef _WIN32

#include "dep_database.h"

#include <gtest/gtest.h>

#include "deplist.h"
#include "test.h"
#include "util.h"

class DepDatabaseTest : public testing::Test {
 public:
  virtual void SetUp() {
    // These tests do real disk accesses, so create a temp dir.
    temp_dir_.CreateAndEnter("Ninja-DepDatabase");
  }

  virtual void TearDown() {
    temp_dir_.Cleanup();
  }

  ScopedTempDir temp_dir_;
};

namespace {

vector<string> GetDepData(DepDatabase& db, const string& name) {
  vector<StringPiece> deps;
  string err;
  db.StartLookups();
  EXPECT_EQ(true, db.FindDepData(name.c_str(), &deps, &err));
  vector<string> result;
  for (vector<StringPiece>::iterator i(deps.begin()); i != deps.end(); ++i) {
    result.push_back(i->AsString());
  }
  db.FinishLookups();
  return result;
}

void StoreDepData(DepDatabase& db, const string& index, const string& dep0) {
  vector<StringPiece> entries;
  entries.push_back(dep0);
  Deplist::WriteDatabase(db, index, entries);
}

}

TEST_F(DepDatabaseTest, Empty) {
  DepDatabase db("depdb", true);
  vector<string> ret = GetDepData(db, "nothing.c");
  ASSERT_EQ(0, ret.size());
}

TEST_F(DepDatabaseTest, AddAndRetrieve) {
  DepDatabase db("depdb", true);
  StoreDepData(db, "a.c", "wee");
  vector<string> ret = GetDepData(db, "a.c");
  EXPECT_EQ(1, ret.size());
  EXPECT_EQ("wee", ret[0]);
}

TEST_F(DepDatabaseTest, AddUpdateRetrieve) {
  DepDatabase db("depdb", true);
  StoreDepData(db, "a.c", "wee");
  StoreDepData(db, "a.c", "blorp");
  vector<string> ret = GetDepData(db, "a.c");
  EXPECT_EQ(1, ret.size());
  EXPECT_EQ("blorp", ret[0]);
}

TEST_F(DepDatabaseTest, AddMultipleSorted) {
  DepDatabase db("depdb", true);
  StoreDepData(db, "a.c", "wee");
  StoreDepData(db, "b.c", "waa");
  StoreDepData(db, "x.c", "woo");
  EXPECT_EQ("wee", GetDepData(db, "a.c")[0]);
  EXPECT_EQ("waa", GetDepData(db, "b.c")[0]);
  EXPECT_EQ("woo", GetDepData(db, "x.c")[0]);
}

TEST_F(DepDatabaseTest, AddMultipleUnsorted) {
  DepDatabase db("depdb", true);
  StoreDepData(db, "x.c", "woo");
  StoreDepData(db, "b.c", "waa");
  StoreDepData(db, "a.c", "wee");
  EXPECT_EQ("wee", GetDepData(db, "a.c")[0]);
  EXPECT_EQ("waa", GetDepData(db, "b.c")[0]);
  EXPECT_EQ("woo", GetDepData(db, "x.c")[0]);
}

TEST_F(DepDatabaseTest, Recompact) {
  DepDatabase db("depdb", true);
}

#endif
