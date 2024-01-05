// Forked from here and improved typings
// https://github.com/gunn/pure-store

import React from 'react'
import { produce } from 'immer'
import { useLayoutEffect } from 'react'
import { useRef } from 'react'

export class PureStore<S, T = S> {
  callbacks = [] as ((s: T) => void)[]
  rootState: T = undefined!!
  getter: (s: S) => T
  root: PureStore<S, S>
  parent: any

  constructor(parent: any | null, getter: (s: S) => T, rootState?: T) {
    this.parent = parent
    this.root = (parent && parent.root) || this
    if (!parent) this.rootState = rootState!!
    this.getter = (s: S) => getter(parent ? parent.getter(s) : s)
  }

  getState = () => this.getter(this.root.rootState)
  get state() {
    return this.getState()
  }

  update = (updater: ((e: T) => void) | Partial<T>) => {
    const updaterFn = updater instanceof Function ? updater : (e: T) => Object.assign(e as any, updater)

    const oldState = this.root.rootState

    this.root.rootState = produce(this.root.rootState, (s: S) => {
      updaterFn(this.getter(s))
    })

    if (this.root.rootState === oldState) return
    this.root.callbacks.forEach((callback: (s: S) => void) => callback(this.root.rootState))
  }

  storeFor = <X>(getter: (s: T) => X): PureStoreReact<T, X> => new PureStoreReact(this, getter)
  updaterFor = <X extends object>(getter: (s: T) => X) => this.storeFor(getter).update

  subscribe = (callback: (s: T) => void) => {
    let oldValue: T = this.getState()

    const getterCallback = (s: S) => {
      const newValue = this.getter(s)
      if (newValue === oldValue) return
      oldValue = newValue
      callback(newValue)
    }

    this.root.callbacks.push(getterCallback)
    return () => {
      const index = this.root.callbacks.indexOf(getterCallback)
      this.root.callbacks.splice(index, 1)
    }
  }

  // Same as subscribe but it also calls the callback with the current state
  // which can be quite useful
  watch = (callback: (s: T) => void) => {
    callback(this.getState())
    return this.subscribe(callback)
  }
}

export const createBasicStore = <S>(state: S) => new PureStore(null, (s: S) => s, state)

export class PureStoreReact<S, T = S> extends PureStore<S, T> {
  usePureStore(): readonly [T, (updater: Partial<T> | ((e: T) => void)) => void]
  usePureStore<X extends object>(getter?: (s: T) => X): readonly [X, (updater: Partial<X> | ((e: X) => void)) => void]
  usePureStore(getter?: any) {
    if (getter) {
      return new PureStoreReact(this, getter).usePureStore()
    }

    // eslint-disable-next-line react-hooks/rules-of-hooks
    const [, setState] = React.useState(this.getState())

    // eslint-disable-next-line react-hooks/rules-of-hooks
    React.useEffect(() => {
      return this.subscribe(() => setState(this.getState()))
    }, [])

    return [this.getState(), this.update] as const
  }

  useValue(): T
  useValue<X>(getter?: (s: T) => X): X
  useValue(getter?: any) {
    return (this.usePureStore(getter) as any)[0]
  }

  useWatch(callback: (x: T) => void) {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const callbackRef = useRef<any>()
    callbackRef.current = callback
    // eslint-disable-next-line react-hooks/rules-of-hooks
    useLayoutEffect(() => {
      return this.watch(() => callbackRef.current(this.getState()))
    }, [])
  }
}

export const createStore = <S>(state: S) => new PureStoreReact(null, (s: S) => s, state)

export type StoreTypeOf<T> = T extends PureStore<infer S, any> ? S : never
