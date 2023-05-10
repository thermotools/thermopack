#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/numpy.h>
#include "Variant2.h"
#include "NotIdGas.h"
#include "Variant1.h"
namespace py = pybind11;
using array_f = pybind11::array_t<float, pybind11::array::c_style | pybind11::array::forcecast>;
using p_array_f = std::optional<pybind11::array_t<float, pybind11::array::c_style | pybind11::array::forcecast>>;

#define BaseEos_bindings(Model) \
    .def("Fideal", [](Model& self, float T, float V, std::vector<float> n, array_f Fid, p_array_f Ft, p_array_f Fv, p_array_f Fn){ \
            auto Fid_bind = Fid.mutable_unchecked().mutable_data(0); \
            auto Ft_bind = (Ft.has_value()) ? Ft.value().mutable_unchecked().mutable_data(0) : nullptr; \
            auto Fv_bind = (Fv.has_value()) ? Fv.value().mutable_unchecked().mutable_data(0) : nullptr; \
            auto Fn_bind = (Fn.has_value()) ? Fn.value().mutable_unchecked().mutable_data(0) : nullptr; \
            self.Fideal(T, V, n.data(), Fid_bind, &Ft_bind, &Fv_bind, &Fn_bind); \
            }, py::arg("T")\
            , py::arg("V")\
            , py::arg("n")\
            , py::arg("Fid").noconvert()\
            , py::arg("Ft").noconvert() = py::none()\
            , py::arg("Fv").noconvert() = py::none()\
            , py::arg("Fn").noconvert() = py::none()\
             \
        ) \
    .def("pressure", [](Model& self, float T, float V, std::vector<float> n, array_f p){ \
            auto p_bind = p.mutable_unchecked().mutable_data(0); \
            self.pressure(T, V, n.data(), p_bind); \
            }, py::arg("T")\
            , py::arg("V")\
            , py::arg("n")\
            , py::arg("p").noconvert()\
             \
        ) \
    .def("set_Tc", &Model::set_Tc) \
    .def("Fres", [](Model& self, float T, float V, std::vector<float> n, array_f Fres, p_array_f Ft, p_array_f Fv, p_array_f Fn){ \
            auto Fres_bind = Fres.mutable_unchecked().mutable_data(0); \
            auto Ft_bind = (Ft.has_value()) ? Ft.value().mutable_unchecked().mutable_data(0) : nullptr; \
            auto Fv_bind = (Fv.has_value()) ? Fv.value().mutable_unchecked().mutable_data(0) : nullptr; \
            auto Fn_bind = (Fn.has_value()) ? Fn.value().mutable_unchecked().mutable_data(0) : nullptr; \
            self.Fres(T, V, n.data(), Fres_bind, &Ft_bind, &Fv_bind, &Fn_bind); \
            }, py::arg("T")\
            , py::arg("V")\
            , py::arg("n")\
            , py::arg("Fres").noconvert()\
            , py::arg("Ft").noconvert() = py::none()\
            , py::arg("Fv").noconvert() = py::none()\
            , py::arg("Fn").noconvert() = py::none()\
             \
        ) 

#define VariantEoS_bindings(Model) \
    .def("variant_common_comp", [](Model& self, array_f mixed_common){ \
            auto mixed_common_bind = mixed_common.mutable_unchecked().mutable_data(0); \
            self.variant_common_comp(mixed_common_bind); \
            }, py::arg("mixed_common").noconvert()\
             \
        ) \
    .def("internal_comp", [](Model& self, float T, float V, const float n[], array_f computed){ \
            auto computed_bind = computed.mutable_unchecked().mutable_data(0); \
            self.internal_comp(T, V, n, computed_bind); \
            }, py::arg("T")\
            , py::arg("V")\
            , py::arg("n")\
            , py::arg("computed").noconvert()\
             \
        ) 


PYBIND11_MODULE(libdemo, handle){
    handle.doc() = "Is this documentation? I have been told it is the best.";

    py::class_<Variant2>(handle, "Variant2") 
        .def(py::init<int, int, float, float, int, float>(), py::arg("ident"), py::arg("nc"), py::arg("Tc"), py::arg("Vc"), py::arg("var1"), py::arg("var2")) 
        VariantEoS_bindings(Variant2) 
        BaseEos_bindings(Variant2); 

    py::class_<NotIdGas>(handle, "NotIdGas") 
        .def(py::init<int, int, float, float>(), py::arg("ident"), py::arg("nc"), py::arg("Tc"), py::arg("Vc")) 
        BaseEos_bindings(NotIdGas); 

    py::class_<Variant1>(handle, "Variant1") 
        .def(py::init<int, int, float, float, int, float>(), py::arg("ident"), py::arg("nc"), py::arg("Tc"), py::arg("Vc"), py::arg("var1"), py::arg("var2")) 
        VariantEoS_bindings(Variant1) 
        BaseEos_bindings(Variant1); 

}
