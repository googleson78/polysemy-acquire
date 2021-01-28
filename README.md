# `polysemy-acquire`

Small adapter to allow using `resourcet`s `Data.Acquire.Acquire` within a `Member (Final IO) r => Sem r a`.

The release function for the `Acquire` will also be called in cases such as `polysemy`s `Error` being thrown and not caught.
