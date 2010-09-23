int main()
{
    // Take care of the special transition buttons.
    switch (button) {
    case stopDigitizingButton_:
        cout << "Digitizing stopped." << endl;
        stopDigitizing();
        return;
        break;
    case manualDigitizingButton_:
        return;
        break;
    case continuousDigitizingButton_:
        firstContinuousPoint_ = true;
        continuousModeList_.clear();
        switchToContinuousMode();
        return;
        break;
    }

    if ( ( ( previousDigitizedPoint_ != point ) && !firstDigitizedPoint_ ) ||
         firstDigitizedPoint_ ) {
        previousDigitizedPoint_ = point;
        const Point2D output_point = transformation_.transform(point);
        cout << output_point << "\t" << point << "\t" << button << endl;
        manualModeList_.append(PairOfPoints(point, output_point));
    }
    firstDigitizedPoint_ = false;
}
