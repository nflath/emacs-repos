template <class T, class U>
void DetectorImp<T, U>::submitOp(AppTransaction*) {}


class DetectOS {
    virtual Detector<SubConfigChange,  AppMO>&
    detector(SubConfigChange* = 0,  AppMO* = 0) const;
    // ...
};


class DetectOS {
    // ...
    virtual Detector<SubConfigChange,  AppMO>&
    detector(SubConfigChange* = 0,  AppMO* = 0) const;
    // ...
};
