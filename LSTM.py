import torch
import torch.nn as nn
import pandas as pd
from torch.utils.data import Dataset, DataLoader
from sklearn.preprocessing import MinMaxScaler
import numpy as np
import random

class TimeSeriesDataset(Dataset):
    def __init__(self, data, sequence_length):
        self.data = torch.FloatTensor(data)
        self.sequence_length = sequence_length

    def __len__(self):
        return len(self.data) - self.sequence_length

    def __getitem__(self, idx):
        return (
            self.data[idx:idx+self.sequence_length],
            self.data[idx+self.sequence_length]
        )

class LSTM(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers):
        super(LSTM, self).__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        
        self.lstm = nn.LSTM(
            input_size=input_size,
            hidden_size=hidden_size,
            num_layers=num_layers,
            batch_first=True
        )
        
        self.fc = nn.Linear(hidden_size, 1)
    
    def forward(self, x):
        # Initialize hidden state with zeros
        h0 = torch.zeros(self.num_layers, x.size(0), self.hidden_size).to(x.device)
        # Initialize cell state
        c0 = torch.zeros(self.num_layers, x.size(0), self.hidden_size).to(x.device)
        
        # Forward propagate LSTM
        out, _ = self.lstm(x, (h0, c0))
        
        # Decode the hidden state of the last time step
        out = self.fc(out[:, -1, :])
        return out

def train_model(model, train_loader, criterion, optimizer, num_epochs):
    model.train()
    for epoch in range(num_epochs):
        total_loss = 0
        for batch_x, batch_y in train_loader:
            batch_x = batch_x.unsqueeze(-1) # Ensure batch_x is 3-D: (batch_size, sequence_length, input_size)
            optimizer.zero_grad()
            outputs = model(batch_x)
            loss = criterion(outputs, batch_y.unsqueeze(1))
            loss.backward()
            optimizer.step()
            total_loss += loss.item()
        
        print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {total_loss/len(train_loader):.4f}')

# After evaluation
def save_results_to_csv(predictions, actuals, ase, horizon_type="short"):
    results_df = pd.DataFrame({
        'Prediction': predictions,
        'Actual': actuals,
        'Error': [pred - act for pred, act in zip(predictions, actuals)]
    })
    
    # Add summary statistics
    summary_df = pd.DataFrame({
        'Metric': ['ASE'],
        'Value': [ase]
    })
    
    # Save to CSV files
    results_df.to_csv(f'predictions_{horizon_type}_term.csv', index=True)
    summary_df.to_csv(f'metrics_{horizon_type}_term.csv', index=False)

# Set seeds for reproducibility
torch.manual_seed(42)
np.random.seed(42)
random.seed(42)
if torch.cuda.is_available():
    torch.cuda.manual_seed(42)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

# Load and preprocess your data
df = pd.read_csv('https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv')
# import pdb
#pdb.set_trace()
data = df['MSPUS'].values[49:246]  # Select rows 49 through 245 inclusive

# After loading data but before normalization
data = np.log(data)  # Apply log transformation

# After loading data but before creating datasets
# Initialize the scaler
scaler = MinMaxScaler()
data_normalized = scaler.fit_transform(data.reshape(-1, 1)).flatten()

# Define parameters
# sequence_length = 10
sequence_length = 8
# sequence_length = 5
hidden_size = 64
# hidden_size = 128
num_layers = 2
# num_layers = 3
batch_size = 32
# batch_size = 16
num_epochs = 100
learning_rate = 0.01
# learning_rate = 0.005
h_short = 4
h_long = 20

# Create dataset and dataloader
# dataset = TimeSeriesDataset(data, sequence_length)
# train_loader = DataLoader(dataset, batch_size=batch_size, shuffle=True)

# Split data into train and test
train_data = data_normalized[:-h_short]  # All data except last h_short entries
test_data = data_normalized[-sequence_length-h_short:]  # Last sequence_length + h_short entries

# Create datasets and dataloaders
train_dataset = TimeSeriesDataset(train_data, sequence_length)
train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
test_dataset = TimeSeriesDataset(test_data, sequence_length)
test_loader = DataLoader(test_dataset, batch_size=1, shuffle=False)

# Initialize model, criterion, and optimizer
model = LSTM(input_size=1, hidden_size=hidden_size, num_layers=num_layers)
# model = LSTM(input_size=1, hidden_size=hidden_size, num_layers=num_layers, dropout=0.2)
criterion = nn.MSELoss()
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)

# Train the model
train_model(model, train_loader, criterion, optimizer, num_epochs)

# Add evaluation mode
def evaluate_model(model, test_loader):
    model.eval()
    predictions = []
    actuals = []
    total_se = 0  # Initialize before the loop
    n = 0  # Initialize before the loop
    with torch.no_grad():
        for batch_x, batch_y in test_loader:
            batch_x = batch_x.unsqueeze(-1)  # Add this line to ensure 3D input
            output = model(batch_x)
            # Denormalize predictions and actuals
            pred = scaler.inverse_transform(output.numpy())[0][0]
            actual = scaler.inverse_transform(batch_y.numpy().reshape(-1, 1))[0][0]
            predictions.append(pred)
            actuals.append(actual)
            # predictions.append(output.item())
            # actuals.append(batch_y.item())

            # Then un-log transform
            pred = np.exp(pred)
            actual = np.exp(actual)

            # Calculate squared error for this prediction
            se = (pred - actual) ** 2
            total_se += se
            n += 1
            
    ase = total_se / 1e6 / n if n > 0 else 0
    return predictions, actuals, ase
    # return predictions, actuals

# Evaluate on test data
# predictions, actuals = evaluate_model(model, test_loader)
predictions, actuals, ase = evaluate_model(model, test_loader)
print("\nTest Results for short term prediction:")
for i, (pred, actual) in enumerate(zip(predictions, actuals)):
    print(f"Prediction {i+1}: {pred:.2f}, Actual: {actual:.2f}")
print(f"\nAverage Squared Error: {ase:,.2f}")

# Save results
save_results_to_csv(predictions, actuals, ase, "short")

# Also do it with the long term evaluation
train_data = data_normalized[:-h_long]  # All data except last h_long entries
test_data = data_normalized[-sequence_length-h_long:]  # Last sequence_length + h_long entries
train_dataset = TimeSeriesDataset(train_data, sequence_length)
train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
test_dataset = TimeSeriesDataset(test_data, sequence_length)
test_loader = DataLoader(test_dataset, batch_size=1, shuffle=False)
train_model(model, train_loader, criterion, optimizer, num_epochs)
predictions, actuals, ase = evaluate_model(model, test_loader)
print("\nTest Results for long term prediction:")
for i, (pred, actual) in enumerate(zip(predictions, actuals)):
    print(f"Prediction {i+1}: {pred:.2f}, Actual: {actual:.2f}")
print(f"\nAverage Squared Error: {ase:,.2f}")
save_results_to_csv(predictions, actuals, ase, "long")