#![cfg_attr(not(feature = "std"), no_std)]

/// Edit this file to define custom logic or remove it if it is not needed.
/// Learn more about FRAME and the core library of Substrate FRAME pallets:
/// <https://substrate.dev/docs/en/knowledgebase/runtime/frame>
pub use pallet::*;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::{dispatch::DispatchResult, pallet_prelude::*};
	use frame_system::pallet_prelude::*;
	use frame_support::sp_runtime::traits::{Hash};
	use frame_support::traits::{Randomness};
	use sp_core::H256;

	// Structs 
	#[derive(Clone, Encode, Decode, Default, PartialEq)]
    pub struct Kitty<Hash, Balance> {
        id: Hash,
        dna: Hash,
        price: Balance,
        gender: Gender,
    }

	/// Configure the pallet by specifying the parameters and types on which it depends.
	#[pallet::config]
	pub trait Config: pallet_balances::Config + frame_system::Config {
		type KittyRandomness: Randomness<H256>;
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
	}

	// Storage

	// Keeps track of the Nonce used in the randomness generator.
	#[pallet::storage]
	#[pallet::getter(fn get_nonce)]
	pub(super) type Nonce<T:Config> = StorageValue<
		_,
		u64,
		ValueQuery,
	>; 

	// Stores a Kitty: it's unique traits and price.
	#[pallet::storage]
	#[pallet::getter(fn kitty)]
	pub(super) type Kitties<T: Config> = StorageMap <
		_,
		Twox64Concat,
		T::Hash,
		Kitty<T::Hash, T::Balance>,
		ValueQuery,
	>;

	// Keeps track of what accounts own what Kitty.
	#[pallet::storage]
	#[pallet::getter(fn owner_of)]
	pub(super) type KittyOwner<T: Config> = StorageMap <
		_,
		Twox64Concat,
		T::Hash,
		Option<T::AccountId>,
		ValueQuery,
	>;

	// Keep track of who a Kitty is owned by.
	#[pallet::storage]
	#[pallet::getter(fn kitty_of_owner_by_index)]
	pub(super) type OwnedKittiesArray<T: Config> = StorageMap <
		_,
		Twox64Concat,
		(T::AccountId, u64),
		T::Hash,
		ValueQuery,
	>;

	// Keeps track of the total amount of Kitties owned.
	#[pallet::storage]
	#[pallet::getter(fn owned_kitty_count)]
	pub(super) type OwnedKittiesCount<T: Config> = StorageMap <
		_,
		Twox64Concat,
		T::AccountId,
		u64,
		ValueQuery,
	>;

	// Keeps track of all owned Kitties by index.
	#[pallet::storage]
	#[pallet::getter(fn owned_kitty_index)]
	pub(super) type OwnedKittiesIndex<T: Config> = StorageMap <
		_,
		Twox64Concat,
		T::Hash,
		u64,
		ValueQuery,
	>;


    // Stores the total amount of Kitties in existence.	
	#[pallet::storage]
	#[pallet::getter(fn all_kitties_count)]
	pub(super) type AllKittiesCount <T: Config> = StorageValue <
		_,
		u64,
		ValueQuery,
	>;

	// An index to track of all Kitties.
	#[pallet::storage]
	#[pallet::getter(fn kitty_by_index)]
	pub(super) type AllKittiesArray <T: Config> = StorageMap <
		_,
		Twox64Concat,
		u64,
		T::Hash,
		ValueQuery,
	>;

	// Keeps track of all the Kitties.  
	#[pallet::storage]
	// #[pallet::getter(fn all_kitties_index)]
	pub(super) type AllKittiesIndex <T: Config> = StorageMap <
		_,
		Twox64Concat,
		T::Hash,
		u64,
		ValueQuery,
	>;

	#[pallet::genesis_config]
    pub struct GenesisConfig<T: Config> {
        pub kitties: Vec<(T::AccountId, T::Hash, T::Balance)>,
    }

	// Required to implement default for GenesisConfig.
    #[cfg(feature = "std")]
    impl<T: Config> Default for GenesisConfig<T> {
        fn default() -> GenesisConfig<T> {
            GenesisConfig { kitties: vec![] }
        }
    }

	#[pallet::genesis_build]
    impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
        fn build(&self) {
            for &(ref acct, hash, balance) in &self.kitties {
                let k = Kitty {
                    id: hash,
                    dna: hash,
                    price: balance,
                    gender: Gender::Male,
                };

                let _ = <Module<T>>::mint(acct.clone(), hash, k);
            }
        }
    }

	#[derive(Encode, Decode, Debug, Clone, PartialEq)]
    pub enum Gender {
        Male,
        Female,
    }

	impl Default for Gender {
        fn default() -> Self {
            Gender::Female
        }
    }

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	// Errors
	#[pallet::error]
	pub enum Error<T> {
		NonceOverflow,
	}

	// Events
	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config>{
		// A new Kitty is Created (owner, kitty_id)
		Created(T::AccountId, T::Hash),
	}

	#[pallet::hooks]
    impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {}

	
	// Dispatchable functions allows users to interact with the pallet and invoke state changes.
	// These functions materialize as "extrinsics", which are often compared to transactions.
	// Dispatchable functions must be annotated with a weight and must return a DispatchResult.
	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::weight(100)]
		pub fn create_kitty(origin: OriginFor<T>) -> DispatchResultWithPostInfo {
			let sender = ensure_signed(origin)?;
			let random_hash = Self::random_hash(&sender);

			let new_kitty = Kitty::<T::Hash, T::Balance> {
                id: random_hash,
                dna: random_hash,
                price: 0u8.into(),
                gender: Kitty::<T, T>::gender(random_hash),
            };

			Self::mint(sender, random_hash, new_kitty)?;
			Self::increment_nonce()?;
			Ok(().into())
		}
	}


	// Helper functions 
	impl<T: Config> Kitty<T, T> {
		pub fn gender(dna: T::Hash) -> Gender {
			if dna.as_ref()[0] % 2 == 0 {
				Gender::Male
			} else {
				Gender::Female
			}
		}
	}	

	// private helper functions
	impl<T: Config> Pallet<T> {
		fn random_hash(sender: &T::AccountId) -> T::Hash {
			let nonce = <Nonce<T>>::get();
			let seed = T::KittyRandomness::random_seed();
			T::Hashing::hash_of(&(seed, &sender, nonce))
		}

		fn mint(
			to: T::AccountId,
			kitty_id: T::Hash,
			new_kitty: Kitty<T::Hash, T::Balance>,
		) -> DispatchResult {
			ensure!(!<KittyOwner<T>>::contains_key(kitty_id),"Kitty already contains key");

			// update total Kitty Counts
			let owned_kitty_count = Self::owned_kitty_count(&to); 
			let new_owned_kitty_count = owned_kitty_count
				.checked_add(1)
				.ok_or("Overflow Adding a new kitty to account balance")?;

			let all_kitties_count = Self::all_kitties_count(); 
			let new_all_kitties_count = all_kitties_count
				.checked_add(1)
				.ok_or("Overflow Adding a new kitty to total supply")?;
	
			// Update storage with new Kitty.
			<Kitties<T>>::insert(kitty_id, new_kitty);
			<KittyOwner<T>>::insert(kitty_id, Some(&to));

			// Write Kitty counting information to storage
			<AllKittiesArray<T>>::insert(new_all_kitties_count, kitty_id);
			<AllKittiesCount<T>>::put(new_all_kitties_count);
            <AllKittiesIndex<T>>::insert(kitty_id, new_all_kitties_count);

            // Write Kitty counting information to storage.
			<OwnedKittiesArray<T>>::insert((to.clone(), new_owned_kitty_count), kitty_id);
            <OwnedKittiesCount<T>>::insert(&to, new_owned_kitty_count);
			<OwnedKittiesIndex<T>>::insert(kitty_id, new_owned_kitty_count);

			// Deposit our "Created" Event
			// Self::deposit_event(Event::Created(to, kitty_id));

			Ok(())
		}

		fn increment_nonce() -> DispatchResult {
			<Nonce<T>>::try_mutate(|nonce| {
				let next = nonce.checked_add(1).ok_or(Error::<T>::NonceOverflow)?;
				*nonce = next;
				Ok(().into())
			})
		}
	}
}
