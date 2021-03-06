// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package nomono checks that a package is monomorphizable.
package nomono

import (
	"errors"
	"go/ast"
	"go/types"
)

func assert(cond bool) {
	if !cond {
		panic("assertion failed")
	}
}

func Check(info *types.Info, files []*ast.File) error {
	w := shared{
		info:      info,
		implicits: make(map[*types.TypeName][]*types.TypeParam),
		canon:     make(map[*types.TypeParam]*types.TypeParam),
		params:    make(map[*types.TypeParam]int),
	}

	for _, file := range files {
		ast.Walk(walker{shared: &w}, file)
	}

	for ident, inst := range w.info.Instances {
		var tparams *types.TypeParamList
		switch obj := w.info.Uses[ident].(type) {
		case *types.Func:
			tparams = obj.Type().(*types.Signature).TypeParams()
		case *types.TypeName:
			tparams = obj.Type().(*types.Named).TypeParams()
		default:
			panic("unexpected object")
		}
		targs := inst.TypeArgs

		assert(tparams.Len() == targs.Len())
		for i := 0; i < tparams.Len(); i++ {
			w.assign(ident, tparams.At(i), targs.At(i))
		}
	}

	nparams := len(w.params)

	Len := make([]int, nparams)
	Dis := make([]int, nparams)
	Enqueued := make([]bool, nparams)
	var Queue []int

	for i := 0; i < nparams; i++ {
		Len[i] = 0
		Dis[i] = 0
		Queue = append(Queue, i)
		Enqueued[i] = true
	}

	for len(Queue) > 0 {
		// u = Queue.pop()
		u := Queue[len(Queue)-1]
		Queue = Queue[:len(Queue)-1]
		Enqueued[u] = false

		for _, edge := range w.edges {
			if edge.src == u {
				v := edge.dst
				x := Dis[u]
				if !edge.direct {
					x--
				}
				if x < Dis[v] {
					Len[v] = Len[u] + 1
					if Len[v] == nparams {
						// TODO(mdempsky): Report the loop.
						return errors.New("found a positive type parameter loop")
					}
					Dis[v] = x
					if !Enqueued[v] {
						Queue = append(Queue, v)
						Enqueued[v] = true
					}
				}
			}
		}
	}

	return nil
}

type shared struct {
	info *types.Info

	// implicits maps defined types to type parameters that are
	// implicitly in scope.
	implicits map[*types.TypeName][]*types.TypeParam

	// canon maps type parameters used by method declarations to the
	// original type parameters from the corresponding type declaration.
	canon map[*types.TypeParam]*types.TypeParam

	params map[*types.TypeParam]int

	edges []edge
}

type walker struct {
	*shared

	scope []*types.TypeParam
}

type edge struct {
	dst    int
	src    int
	direct bool
}

func (w walker) Visit(n ast.Node) ast.Visitor {
	switch n := n.(type) {
	case *ast.FuncDecl:
		obj := w.info.Defs[n.Name].(*types.Func)
		sig := obj.Type().(*types.Signature)

		recv := sig.Recv()
		if recv == nil {
			return w.extend(sig.TypeParams())
		}

		typ, _ := decompose(deref(sig.Recv().Type()).(*types.Named))

		mparams := sig.RecvTypeParams()
		tparams := typ.TypeParams()
		for i := 0; i < mparams.Len(); i++ {
			w.canon[mparams.At(i)] = tparams.At(i)
		}

		return w.extend(mparams)

	case *ast.TypeSpec:
		obj, ok := w.info.Defs[n.Name].(*types.TypeName)
		if ok && len(w.scope) != 0 {
			w.implicits[obj] = w.scope
		}

		return w.extend(obj.Type().(*types.Named).TypeParams())
	}

	return w
}

func (w walker) extend(params *types.TypeParamList) walker {
	if params.Len() == 0 {
		return w
	}

	w.scope = w.scope[:len(w.scope):len(w.scope)]
	for i := 0; i < params.Len(); i++ {
		w.scope = append(w.scope, params.At(i))
	}

	return w
}

func (w *shared) assign(ident *ast.Ident, param *types.TypeParam, arg types.Type) {
	var do func(typ types.Type)
	do = func(typ types.Type) {
		switch typ := typ.(type) {
		default:
			panic("unexpected type")

		case *types.TypeParam:
			w.flow(ident, param, typ, typ == arg)

		case *types.Named:
			typ, targs := decompose(typ)
			for _, implicit := range w.implicits[typ.Obj()] {
				do(implicit)
			}
			for i := 0; i < targs.Len(); i++ {
				do(targs.At(i))
			}

		case *types.Array:
			do(typ.Elem())
		case *types.Basic:
			// ok
		case *types.Chan:
			do(typ.Elem())
		case *types.Map:
			do(typ.Key())
			do(typ.Elem())
		case *types.Pointer:
			do(typ.Elem())
		case *types.Slice:
			do(typ.Elem())

		case *types.Interface:
			for i := 0; i < typ.NumMethods(); i++ {
				do(typ.Method(i).Type())
			}
		case *types.Signature:
			tuple := func(tup *types.Tuple) {
				for i := 0; i < tup.Len(); i++ {
					do(tup.At(i).Type())
				}
			}
			tuple(typ.Params())
			tuple(typ.Results())
		case *types.Struct:
			for i := 0; i < typ.NumFields(); i++ {
				do(typ.Field(i).Type())
			}
		}
	}
	do(arg)
}

func (w *shared) flow(ident *ast.Ident, dst, src *types.TypeParam, direct bool) {
	w.edges = append(w.edges, edge{w.index(dst), w.index(src), direct})
}

func (w *shared) index(param *types.TypeParam) int {
	if orig, ok := w.canon[param]; ok {
		param = orig
	}

	idx, ok := w.params[param]
	if !ok {
		idx = len(w.params)
		w.params[param] = idx
	}
	return idx
}

func deref(typ types.Type) types.Type {
	if ptr, ok := typ.Underlying().(*types.Pointer); ok {
		return ptr.Elem()
	}
	return typ
}

func decompose(named *types.Named) (*types.Named, *types.TypeList) {
	return named.Origin(), named.TypeArgs()
}
