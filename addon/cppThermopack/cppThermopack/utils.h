#pragma once
#include <vector>
#include <string>
#include <iostream>

using vector1d = std::vector<double>;
using vector2d = std::vector<vector1d>;

enum PropertyFlag{
    total = 0,
    residual,
    ideal
};

enum Phase{
    two = 0,
    liq,
    vap,
    fake,
    mingibbs,
    solid
};

inline void check_has_diff(bool has_diff, std::string diff){
    if (!has_diff) throw std::runtime_error("Property does not have differential " + diff);
}

class Property{
    public:
    inline operator double() const {return value_;}
    inline double operator=(double val){value_ = val; return *this;}
    inline double value() const {return value_;}
    inline double dt() const {check_has_diff(dt_ptr, "dt"); return dt_;}
    inline double dv() const {check_has_diff(dv_ptr, "dv"); return dv_;}
    inline double dp() const {check_has_diff(dp_ptr, "dp"); return dp_;}
    inline vector1d dn() const {check_has_diff(dn_ptr, "dn"); return dn_;}

    friend std::ostream& operator<<(std::ostream& strm, const Property& self){
        strm << "Property with value : " << self.value_ << "\n";
        if (self.dt_ptr) strm << "\tdt : " << self.dt_ << "\n";
        if (self.dv_ptr) strm << "\tdv : " << self.dv_ << "\n";
        if (self.dp_ptr) strm << "\tdp : " << self.dp_ << "\n";
        if (self.dn_ptr){
            strm << "\tdn : ";
            for (double dni : self.dn_) strm << dni << ", ";
            strm << "\n";
        }
        return strm;
    }

    friend class Thermo;
    private:
    Property(size_t nc, bool dt, bool dv, bool dp, bool dn) 
        : dn_(nc, 0.)
    {
        dt_ptr = dt ? &dt_ : nullptr;
        dv_ptr = dv ? &dv_ : nullptr;
        dp_ptr = dp ? &dp_ : nullptr;
        dn_ptr = dn ? dn_.data() : nullptr;
    } 

    inline double** dt_p(){return &dt_ptr;}
    double value_;
    double dt_, dv_, dp_;
    vector1d dn_;

    double *dt_ptr, *dv_ptr, *dp_ptr, *dn_ptr;
};

class VectorProperty{
    public:
    inline operator vector1d() const {return value_;}
    inline vector1d value() const {return value_;}
    inline vector1d dt() const {check_has_diff(dt_ptr, "dt"); return dt_;}
    inline vector1d dv() const {check_has_diff(dv_ptr, "dv"); return dv_;}
    inline vector1d dp() const {check_has_diff(dp_ptr, "dp"); return dp_;}
    inline vector2d dn() const {
        check_has_diff(dn_ptr, "dn"); 
        size_t nvals = value_.size();
        vector2d dn_2d(nvals, vector1d(nvals, 0.));
        for (size_t i = 0; i < nvals; i++){
            for (size_t j = 0; j < nvals; j++){
                dn_2d[i][j] = dn_[j * nvals + i]; // dn_ uses Fortran-ordering internally
            }
        }
        return dn_2d;
    }

    friend std::ostream& operator<<(std::ostream& strm, const VectorProperty& self){
        strm << "VectorProperty with value : ";
        for (double d : self.value_) strm << d << ", ";
        strm << "\n";
        if (self.dt_ptr){strm << "\tdt : "; for (double d : self.dt_) strm << d << ", "; strm << "\n";}
        if (self.dv_ptr){strm << "\tdv : "; for (double d : self.dv_) strm << d << ", "; strm << "\n";}
        if (self.dp_ptr){strm << "\tdp : "; for (double d : self.dp_) strm << d << ", "; strm << "\n";}
        if (self.dn_ptr){
            strm << "\tdn : [\n";
            for (vector1d line : self.dn()){
                strm << "\t\t[ ";
                for (double d : line){
                    strm << d << ", ";
                }
                strm << "]\n";
            }
            strm << "\t     ]\n";
        }
        return strm;
    }

    friend class Thermo;
    private:
    VectorProperty(size_t size, bool dt, bool dv, bool dp, bool dn)
        : value_(size, 0.), dt_(size, 0.), dv_(size, 0.), dp_(size, 0.), dn_(size * size, 0.)
    {   
        dt_ptr = dt ? dt_.data() : nullptr;
        dv_ptr = dv ? dv_.data() : nullptr;
        dp_ptr = dp ? dp_.data() : nullptr;
        dn_ptr = dn ? dn_.data() : nullptr;
    }

    vector1d value_;
    vector1d dt_, dv_, dp_, dn_;
    double *dt_ptr, *dv_ptr, *dp_ptr, *dn_ptr;
  
};