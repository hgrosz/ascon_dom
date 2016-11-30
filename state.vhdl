-------------------------------------------------------------------------------
-- Title      : Ascon state with round transformation
-- Project    : 
-------------------------------------------------------------------------------
-- File       : state.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-18
-- Last update: 2016-11-29
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-18  1.0      hgross	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ascon_pkg.all;
-------------------------------------------------------------------------------

entity state is

  generic (
    DATA_BLOCK_SIZE : integer := 64;     -- 64 for Ascon128, 128 for Ascon128a
    KEY_SIZE        : integer := 128;    -- only 128 supported so far
    ROUNDS_A        : integer := 12;     -- 12 is default
    ROUNDS_B        : integer := 6;      -- 6 for Ascon128, 8 for Ascon128a
    SBOX_INSTANCES  : integer := 64;      -- Parallel Sboxes = 1,2,4,8,16,32,64
    IMPLICIT_AFFINE : string  := "yes";  -- Sbox affine transform. shall be
                                         -- done during round const addition?
    PIPELINED       : string  := "yes"   -- "yes", or "no"
    );

  port (
    ClkxCI                     : in  std_logic;
    RstxBI                     : in  std_logic;
    -- State Inputs
    KeyxDI                     : in  t_shared_key_var(D downto 0);
    NoncexDI                   : in  std_logic_vector(127 downto 0);
    DataxDI                    : in  t_shared_data(D downto 0);
    RoundConstxDI              : in  std_logic_vector(3 downto 0);
    -- Fresh random Z masks
    Z0xDI                      : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z1xDI                      : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z2xDI                      : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z3xDI                      : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z4xDI                      : in  random_t(SBOX_INSTANCES-1 downto 0);
    -- Control the state
    DoInitializeStatexSI       : in  std_logic;
    DoRoundTransfromationxSI   : in  std_logic;
    isEncryptionxSI            : in  std_logic;
    addedKeyAtInitxSI          : in  std_logic;
    addedDomainSeparationxSI   : in  std_logic;
    isFirstRoundOfTransformxSI : in  std_logic;
    FSMxDI                     : in  ascon_fsm_t;
    -- State Outputs
    DataxDO                    : out t_shared_data(D downto 0);
    DataReadyxSO               : out std_logic;
    TagxDO                     : out t_shared_tag (D downto 0);
    doIncrementRoundCounterxSO : out std_logic;
    StateBusyxSO               : out std_logic
    );

end entity state;

-------------------------------------------------------------------------------

architecture str of state is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  -- State registers
  signal X0xDN, X1xDN, X2xDN, X3xDN, X4xDN : t_shared_state_var(D downto 0);
  signal X0xDP, X1xDP, X2xDP, X3xDP, X4xDP : t_shared_state_var(D downto 0);

  -- Internal FSM
  type state_fsm_t is (IDLE, ADD_ROUND_CONST, SBOX_LAYER, LINEAR_LAYER);
  signal StateFSMxDN, StateFSMxDP : state_fsm_t;

  -- Internal control signals
  signal ResetCounterxS : std_logic;    -- reset Sbox counter
  signal SboxCounterxD  : integer range 0 to 127;

  -- Signals for Sboxes
  signal X0SboxInxD   : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X1SboxInxD   : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X2SboxInxD   : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X3SboxInxD   : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X4SboxInxD   : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X0SboxOutxD  : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X1SboxOutxD  : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X2SboxOutxD  : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X3SboxOutxD  : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  signal X4SboxOutxD  : sbox_data_t(SBOX_INSTANCES-1 downto 0);
  
  -- Use generics as logic vectors
  constant CONST_KEY_SIZE  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
  constant CONST_ROUNDS_A  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
  constant CONST_ROUNDS_B  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));
  constant CONST_RATE      : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(DATA_BLOCK_SIZE, 8));

begin  -- architecture str

  -----------------------------------------------------------------------------
  -- State FSM + control logic
  -----------------------------------------------------------------------------
  state_fsm_p : process (DoRoundTransfromationxSI, SboxCounterxD, StateFSMxDP) is
  begin  -- process state_fsm_p
    StateFSMxDN                <= StateFSMxDP;  -- default
    ResetCounterxS             <= '1';
    doIncrementRoundCounterxSO <= '0';
    StateBusyxSO               <= '1';

    -----------------------
    -- State transitions --
    -----------------------
    case StateFSMxDP is
      -- *** IDLE ***
      when IDLE =>
        if (DoRoundTransfromationxSI = '1') then
          StateFSMxDN <= ADD_ROUND_CONST;
        end if;
        StateBusyxSO <= '0'; -- ready for next input

      -- *** ADD_ROUND_CONST ***
      when ADD_ROUND_CONST =>
        StateFSMxDN                <= SBOX_LAYER;
        doIncrementRoundCounterxSO <= '1'; -- RoundCounter + 1

      -- *** SBOX_LAYER ***
      when SBOX_LAYER =>
        -- Implicite affine calculation saves one cycle
        if ((IMPLICIT_AFFINE = "yes")and SboxCounterxD >= (64/SBOX_INSTANCES)) or
           ((IMPLICIT_AFFINE = "no") and SboxCounterxD >= (64/SBOX_INSTANCES) + 1)then
          StateFSMxDN  <= LINEAR_LAYER;
          StateBusyxSO <= '0';          -- ready for next input
        end if;
        ResetCounterxS <= '0';          -- Sbox steps counter + 1

      -- *** LINEAR_LAYER ***
      when LINEAR_LAYER =>
        -- Directly perform state transfromation if already ready
        if (DoRoundTransfromationxSI = '1') then
          StateFSMxDN <= ADD_ROUND_CONST;
        else
          StateFSMxDN <= IDLE;
        end if;
        StateBusyxSO <= '0';            -- ready for next input
        
      when others => null;
    end case;  -- State transitions
  end process state_fsm_p;

  -----------------------------------------------------------------------------
  -- State transformation and data output logic
  -----------------------------------------------------------------------------
  state_transfromations_p : process (DoInitializeStatexSI, KeyxDI, NoncexDI,
                                    RoundConstxDI, StateFSMxDP, X0SboxOutxD,
                                    X0xDP, X1SboxOutxD, X1xDP, X2SboxOutxD,
                                    X2xDP, X3SboxOutxD, X3xDP, X4SboxOutxD,
                                    X4xDP, FSMxDI, isFirstRoundOfTransformxSI,
                                    addedDomainSeparationxSI, DataxDI,
                                    addedKeyAtInitxSI) is
    -- For more complex state transfroamtion these variables are used
    variable X0, X1, X2, X3, X4 : t_shared_state_var(D downto 0);
  begin  -- process state_transfromations_p
    for i in D downto 0 loop
      --default
      X0xDN(i) <= X0xDP(i);
      X1xDN(i) <= X1xDP(i);
      X2xDN(i) <= X2xDP(i);
      X3xDN(i) <= X3xDP(i);
      X4xDN(i) <= X4xDP(i);

      --------------------------
      -- State transformation --
      --------------------------
      case StateFSMxDP is
        -- *** IDLE ***
        when IDLE =>
          -- Initialize state
          if DoInitializeStatexSI = '1' then
            -- only for first share
            if i = 0 then
              X0xDN(0) <= CONST_KEY_SIZE & CONST_RATE & CONST_ROUNDS_A & CONST_ROUNDS_B & ZEROS(32);

              -- Nonce is unshared (public value)
              X3xDN(0) <= NoncexDI(127 downto 64);
              X4xDN(0) <= NoncexDI(63 downto 0);
            else
              X0xDN(i) <= (others => '0');
              X3xDN(i) <= (others => '0');
              X4xDN(i) <= (others => '0');
            end if;

            -- Key is shared
            X1xDN(i) <= KeyxDI(i)(127 downto 64);
            X2xDN(i) <= KeyxDI(i)(63 downto 0);
          end if;

        -- *** ADD_ROUND_CONST ***
        when ADD_ROUND_CONST =>

          -- default
          X0(i) := X0xDP(i);
          X1(i) := X1xDP(i);
          X2(i) := X2xDP(i);
          X3(i) := X3xDP(i);
          X4(i) := X4xDP(i);

          -- add round constant to X2
          X2(0)(7 downto 0) := X2(0)(7 downto 0) xor (not RoundConstxDI & RoundConstxDI);

          -- add Associated data or PT or replace with CT
          if isFirstRoundOfTransformxSI = '1' then  -- first round?
            -- XOR when associated data or PT
            if FSMxDI = ASSOCIATED_DATA or
              ((FSMxDI = PTCT_DATA or FSMxDI = FINALIZE) and isEncryptionxSI = '1') then
              if (VARIANT = Ascon128) then          -- Ascon128 
                X0(i) := X0(i) xor DataxDI(i);
              else
                X0(i) := X0(i) xor DataxDI(i)(127 downto 64);
                X1(i) := X1(i) xor DataxDI(i)(63 downto 0);
              end if;
            -- Replace when CT
            elsif (FSMxDI = PTCT_DATA or FSMxDI = FINALIZE) and isEncryptionxSI = '0' then  
              if (VARIANT = Ascon128) then          -- Ascon128 
                X0(i) := DataxDI(i);
              else
                X0(i) := DataxDI(i)(127 downto 64);
                X1(i) := DataxDI(i)(63 downto 0);
              end if;
            end if;
          end if;

          -- add 0*||K after init
          if (addedKeyAtInitxSI = '0') and (isFirstRoundOfTransformxSI = '1') then
            X3(i) := X3(i) xor KeyxDI(i)(127 downto 64);
            X4(i) := X4(i) xor KeyxDI(i)(63 downto 0);
          end if;

          -- add 0*||1 when processing first plaintext/ciphertext
          if (FSMxDI = PTCT_DATA or FSMxDI = FINALIZE) and
            addedDomainSeparationxSI = '0' and isFirstRoundOfTransformxSI = '1' then
            -- invert only last bit of first share
            X4(0)(0) := X4(0)(0) xor '1';
          end if;

          -- add K||0* when doing finalization
          if (FSMxDI = FINALIZE) and (RoundConstxDI = "0000") then
            if (VARIANT = Ascon128) then  -- Ascon128        
              X1(i) := X1(i) xor KeyxDI(i)(127 downto 64);
              X2(i) := X2(i) xor KeyxDI(i)(63 downto 0);

            else                        -- Ascon128a
              X2(i) := X2(i) xor KeyxDI(i)(127 downto 64);
              X3(i) := X3(i) xor KeyxDI(i)(63 downto 0);
            end if;
          end if;

          -- do affine transformation from Sbox
          if (IMPLICIT_AFFINE = "yes") then
            X0(i) := X0(i) xor X4(i);
            X2(i) := X2(i) xor X1(i);
            X4(i) := X4(i) xor X3(i);
          end if;

          -- Write back variables
          X0xDN(i) <= X0(i);
          X1xDN(i) <= X1(i);
          X2xDN(i) <= X2(i);
          X3xDN(i) <= X3(i);
          X4xDN(i) <= X4(i);

        -- *** SBOX_LAYER ***
        when SBOX_LAYER =>
          -- Shift state right according to number of sboxes in parallel
          for bit_index in 63 downto 0 loop
            -- Shift in bits from state
            if bit_index < (64-SBOX_INSTANCES) then
              X0xDN(i)(bit_index) <= X0xDP(i)(bit_index + SBOX_INSTANCES);
              X1xDN(i)(bit_index) <= X1xDP(i)(bit_index + SBOX_INSTANCES);
              X2xDN(i)(bit_index) <= X2xDP(i)(bit_index + SBOX_INSTANCES);
              X3xDN(i)(bit_index) <= X3xDP(i)(bit_index + SBOX_INSTANCES);
              X4xDN(i)(bit_index) <= X4xDP(i)(bit_index + SBOX_INSTANCES);
            else
              -- Shift in bits from sboxes
              X0xDN(i)(bit_index) <= X0SboxOutxD(bit_index - 64 + SBOX_INSTANCES)(i);
              X1xDN(i)(bit_index) <= X1SboxOutxD(bit_index - 64 + SBOX_INSTANCES)(i);
              X2xDN(i)(bit_index) <= X2SboxOutxD(bit_index - 64 + SBOX_INSTANCES)(i);
              X3xDN(i)(bit_index) <= X3SboxOutxD(bit_index - 64 + SBOX_INSTANCES)(i);
              X4xDN(i)(bit_index) <= X4SboxOutxD(bit_index - 64 + SBOX_INSTANCES)(i);
            end if;
          end loop;

        -- *** LINEAR_LAYER ***
        when LINEAR_LAYER =>
          X0xDN(i) <= X0xDP(i) xor ROTATE_STATE_WORD(X0xDP(i), 19) xor ROTATE_STATE_WORD(X0xDP(i), 28);
          X1xDN(i) <= X1xDP(i) xor ROTATE_STATE_WORD(X1xDP(i), 61) xor ROTATE_STATE_WORD(X1xDP(i), 39);
          X2xDN(i) <= X2xDP(i) xor ROTATE_STATE_WORD(X2xDP(i), 1) xor ROTATE_STATE_WORD(X2xDP(i), 6);
          X3xDN(i) <= X3xDP(i) xor ROTATE_STATE_WORD(X3xDP(i), 10) xor ROTATE_STATE_WORD(X3xDP(i), 17);
          X4xDN(i) <= X4xDP(i) xor ROTATE_STATE_WORD(X4xDP(i), 7) xor ROTATE_STATE_WORD(X4xDP(i), 41);
          
        when others => null;
      end case;  -- State transitions


      ----------------
      -- Sbox input --
      ----------------
      -- Input of Sboxes is always the LSB's of the state shares
      for sbox_i in SBOX_INSTANCES-1 downto 0 loop
        X0SboxInxD(sbox_i)(i) <= X0xDP(i)(sbox_i);
        X1SboxInxD(sbox_i)(i) <= X1xDP(i)(sbox_i);
        X2SboxInxD(sbox_i)(i) <= X2xDP(i)(sbox_i);
        X3SboxInxD(sbox_i)(i) <= X3xDP(i)(sbox_i);
        X4SboxInxD(sbox_i)(i) <= X4xDP(i)(sbox_i);
      end loop;

      -------------------------------
      -- Data and Tag output logic --
      -------------------------------
      -- Data can be read in ADD_ROUND_CONST state
      -- or during READ_TAG state when last transfromation was done
      if ((isFirstRoundOfTransformxSI = '1') and (StateFSMxDP = ADD_ROUND_CONST)) or
        ((StateFSMxDP = IDLE) and (FSMxDI = READ_TAG)) then
        DataReadyxSO <= '1';
      else
        DataReadyxSO <= '0';
      end if;

      -- Data output
      if (VARIANT = Ascon128) then
        DataxDO(i) <= X0xDP(i) xor DataxDI(i);
      else
        DataxDO(i) <= (X0xDP(i) & X1xDP(i)) xor DataxDI(i);
      end if;

      -- Tag output
      TagxDO(i) <= (X3xDP(i) & X4xDP(i)) xor KeyxDI(i);
    end loop;  -- i
  end process state_transfromations_p;

  -----------------------------------------------------------------------------
  -- Generate S-boxes
  -----------------------------------------------------------------------------
  generate_sboxes_g: for sbox_i in SBOX_INSTANCES-1 downto 0 generate
    generated_sbox: entity work.sbox
      generic map (
        SKIP_AFFINE => IMPLICIT_AFFINE,
        PIPELINED   => PIPELINED,
        D           => D)
      port map (
        ClkxCI => ClkxCI,
        RstxBI => RstxBI,
        X0xDI  => X0SboxInxD(sbox_i),
        X1xDI  => X1SboxInxD(sbox_i),
        X2xDI  => X2SboxInxD(sbox_i),
        X3xDI  => X3SboxInxD(sbox_i),
        X4xDI  => X4SboxInxD(sbox_i),
        Z0xDI  => Z0xDI(sbox_i),
        Z1xDI  => Z1xDI(sbox_i),
        Z2xDI  => Z2xDI(sbox_i),
        Z3xDI  => Z3xDI(sbox_i),
        Z4xDI  => Z4xDI(sbox_i),
        X0xDO  => X0SboxOutxD(sbox_i),
        X1xDO  => X1SboxOutxD(sbox_i),
        X2xDO  => X2SboxOutxD(sbox_i),
        X3xDO  => X3SboxOutxD(sbox_i),
        X4xDO  => X4SboxOutxD(sbox_i));
  end generate generate_sboxes_g;
  
  -----------------------------------------------------------------------------
  -- Register processes
  -----------------------------------------------------------------------------
  register_p : process (ClkxCI, RstxBI) is
  begin  -- process register_p
    for i in D downto 0 loop
      if RstxBI = '0' then              -- asynchronous reset (active low)
        X0xDP(i)    <= (others => '0');
        X1xDP(i)    <= (others => '0');
        X2xDP(i)    <= (others => '0');
        X3xDP(i)    <= (others => '0');
        X4xDP(i)    <= (others => '0');
        StateFSMxDP <= IDLE;
      elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
        X0xDP(i)    <= X0xDN(i);
        X1xDP(i)    <= X1xDN(i);
        X2xDP(i)    <= X2xDN(i);
        X3xDP(i)    <= X3xDN(i);
        X4xDP(i)    <= X4xDN(i);
        StateFSMxDP <= StateFSMxDN;
      end if;
    end loop;  -- i
  end process register_p;

  -----------------------------------------------------------------------------
  -- S-box steps counter
  -----------------------------------------------------------------------------
  sbox_counter_p: process (ClkxCI, ResetCounterxS, RstxBI) is
  begin  -- process sbox_counter_p
    if RstxBI = '0' or  ResetCounterxS = '1' then -- asynchronous reset (active low)
      SboxCounterxD <= 0;
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      SboxCounterxD <= SboxCounterxD + 1;
    end if;
  end process sbox_counter_p;

end architecture str;

-------------------------------------------------------------------------------
