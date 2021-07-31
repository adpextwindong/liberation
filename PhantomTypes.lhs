Can we dispatch on a phantom type? For example chosing between gfx backends.

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

data GFXBackend a = GFXBackend

data OpenGL
data Vulkan

mkGLBackend :: GFXBackend OpenGL
mkGLBackend = GFXBackend

mkVulkanBackend :: GFXBackend Vulkan
mkVulkanBackend = GFXBackend

------------------------------------
gluLookAt :: GFXBackend OpenGL -> IO ()
gluLookAt = undefined

vkLook :: GFXBackend Vulkan -> IO ()
vkLook = undefined
------------------------------------

foo :: GFXBackend a -> IO ()
foo (bk :: GFXBackend OpenGL) = gluLookAt bk
foo (bk :: GFXBackend vkLook) = vkLook bk
foo _ = undefined

\end{code}

```
PhantomTypes.lhs:26:6: error:
    * Couldn't match type `a' with `OpenGL'
      `a' is a rigid type variable bound by
        the type signature for:
          foo :: forall a. GFXBackend a -> IO ()
        at PhantomTypes.lhs:25:1-28
      Expected type: GFXBackend a
        Actual type: GFXBackend OpenGL
    * When checking that the pattern signature: GFXBackend OpenGL
        fits the type of its context: GFXBackend a
      In the pattern: bk :: GFXBackend OpenGL
      In an equation for `foo':
          foo (bk :: GFXBackend OpenGL) = gluLookAt bk
    * Relevant bindings include
        foo :: GFXBackend a -> IO () (bound at PhantomTypes.lhs:26:1)
```

TODO do a version of the code before applying ScopedTypeVariables extension usage in foo to build up to this version
TODO riff off this Singleton pattern

https://gist.github.com/gallais/b08d7ba495ea2e91968c
