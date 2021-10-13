// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package nomono_test

import (
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"testing"

	"github.com/mdempsky/nomono"
)

var goods = []string{
	"func F[T any](x T) { F(x) }",
	"func F[T, U, V any]() { F[U, V, T](); F[V, T, U]() }",
	"type Ring[A, B, C any] struct { L *Ring[B, C, A]; R *Ring[C, A, B] }",
	"func F[T any]() { type U[T any] [unsafe.Sizeof(F[*T])]byte }",
	"func F[T any]() { type U[T any] [unsafe.Sizeof(F[*T])]byte; var _ U[int] }",
	"type U[T any] [unsafe.Sizeof(F[*T])]byte; func F[T any]() { var _ U[U[int]] }",
}

var bads = []string{
	// issue #48951
	// "type A[T any] struct { _ A[*T] }",

	"func F[T any](x T) { F(&x) }",
	"func F[T any]() { F[*T]() }",
	"func F[T any]() { F[[]T]() }",
	"func F[T any]() { F[[1]T]() }",
	"func F[T any]() { F[chan T]() }",
	"func F[T any]() { F[map[*T]int]() }",
	"func F[T any]() { F[map[int]T]() }",
	"func F[T any]() { F[func(T)]() }",
	"func F[T any]() { F[func() T]() }",
	"func F[T any]() { F[struct{ t T }]() }",
	"func F[T any]() { F[interface{ t() T }]() }",
	"type U[_ any] int; func F[T any]() { F[U[T]]() }",
	"func F[T any]() { type U int; F[U]() }",
	"type U[T any] int; func (U[T]) m() { var _ U[*T] }",
	"type U[T any] int; func (*U[T]) m() { var _ U[*T] }",
	"type U[T any] [unsafe.Sizeof(F[*T])]byte; func F[T any]() { var _ U[T] }",
	"func F[A, B, C, D, E any]() { F[B, C, D, E, *A]() }",
}

func check(t *testing.T, body string) error {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "x.go", "package x; import `unsafe`; var _ unsafe.Pointer;\n"+body, 0)
	if err != nil {
		t.Fatal(err)
	}
	files := []*ast.File{file}

	conf := types.Config{
		Importer: importer.Default(),
	}
	info := types.Info{
		Defs:      make(map[*ast.Ident]types.Object),
		Uses:      make(map[*ast.Ident]types.Object),
		Types:     make(map[ast.Expr]types.TypeAndValue),
		Instances: make(map[*ast.Ident]types.Instance),
	}
	_, err = conf.Check("x", fset, files, &info)
	if err != nil {
		t.Fatal(err)
	}

	return nomono.Check(&info, files)
}

func TestCheck(t *testing.T) {
	for i, good := range goods {
		if err := check(t, good); err != nil {
			t.Errorf("%d: unexpected error: %v", i, err)
		}
	}
}

func TestBads(t *testing.T) {
	for i, bad := range bads {
		if err := check(t, bad); err == nil {
			t.Errorf("%d: unexpected success", i)
		}
	}
}
