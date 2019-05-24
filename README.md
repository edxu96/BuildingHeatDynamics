# BuildingHeatDynamics

Explore Building Heat Dynamics using ARIMAX and State Space (Kalman Filter) Model

There is a building with floor heating. To keep the indoor temperature steady, the power of the floor heating will adjust automatically. The control strategy can be very simple, like to adjust according to current indoor temperature. However, this usually causes significant fluctuations. It's better to adjust according to the prediction. The heat feature of the building is a dynamical system, so one advanced regression model and three dynamical models are used to model and predict the heat dynamics.

![Reconstruction and Prediction of Indoor Temperature using Kalman Filter (last 10 observations and 3 predictions)](StateSpaceKalmanFilter/Image/104.png)

Detailed explaination can be found in [edxu96/BuildingHeatDynamics/wiki](https://github.com/edxu96/BuildingHeatDynamics/wiki/1-Home "edxu96/BuildingHeatDynamics/wiki").
