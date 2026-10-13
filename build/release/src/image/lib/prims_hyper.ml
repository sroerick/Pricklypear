(** Pricklypear Image — hypermedia harness shim.

    The soup/ tree prims + the host-agnostic helpers (assoc_of,
    render_handler_value) were lifted into the shared pure-prims layer
    [pure/prims_soup.ml] so both the image and the agent register the
    SAME tree prims (spec: hypermedia-harness.kernel-prims SHARED
    KERNEL). This file re-exports them so image call sites
    (Prims_hyper.register_pure_prims / assoc_of / render_handler_value)
    stay unchanged.

    The session-capturing prim route/dispatch stays registered INLINE in
    session.ml (register_hyper_route_dispatch, right after this module's
    register_pure_prims call) so it can close over Session.t without a
    dependency cycle — mirrors register_crud_prims / register_net_prims. *)
include Prims_soup
